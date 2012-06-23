{-# LANGUAGE CPP #-}
-- | Need to prevent output to the terminal, a file, or stderr? Need to capture it and use it for
-- your own means? Now you can, with 'silence' and 'capture'.

module System.IO.Silently (
  silence, hSilence,
  capture, hCapture
) where

import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO
import Control.Exception (bracket)
import Control.DeepSeq
import System.Directory (removeFile,getTemporaryDirectory)

nullDevice :: FilePath
#ifdef WINDOWS
nullDevice = "NUL"
#elif UNIX
nullDevice = "/dev/null"
#else
nullDevice = undefined -- FIXME
#endif

-- | Run an IO action while preventing all output to stdout.
silence :: IO a -> IO a
silence = hSilence [stdout]

-- | Run an IO action while preventing all output to the given handles.
hSilence :: [Handle] -> IO a -> IO a
hSilence handles action = bracket (openFile nullDevice AppendMode)
                             hClose
                             prepareAndRun


 where
  prepareAndRun tmpHandle = go handles
    where
      go [] = action
      go hs = goBracket go tmpHandle hs


getTempOrCurrentDirectory :: IO String
getTempOrCurrentDirectory = getTemporaryDirectory `Prelude.catch` (\ex -> return ".")

-- | Run an IO action while preventing and capturing all output to stdout.
-- This will, as a side effect, create and delete a temp file in the temp directory or current directory if there is no temp directory.
capture :: IO a -> IO (String, a)
capture = hCapture [stdout]

-- | Run an IO action while preventing and capturing all output to the given handles.
-- This will, as a side effect, create and delete a temp file in the temp directory or current directory if there is no temp directory.
hCapture :: [Handle] -> IO a -> IO (String, a)
hCapture handles action = do
  tmpDir <- getTempOrCurrentDirectory
  bracket (openTempFile tmpDir "capture")
                             cleanup
                             prepareAndRun
 where
  cleanup (tmpFile,tmpHandle) = do
    hClose tmpHandle
    removeFile tmpFile
  prepareAndRun (tmpFile,tmpHandle) = go handles
    where
      go [] = do
              a <- action
              mapM_ hFlush handles
              hClose tmpHandle
              str <- readFile tmpFile
              str `deepseq` return (str,a)
      go hs = goBracket go tmpHandle hs

goBracket :: ([Handle] -> IO a) -> Handle -> [Handle] -> IO a
goBracket go tmpHandle (h:hs) = bracket (do old <- hDuplicate h
                                            hDuplicateTo tmpHandle h
                                            return old)
                                        (\old -> hDuplicateTo old h >> hClose old)
                                        (\_   -> go hs)
