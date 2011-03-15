
-- | Need to prevent output to the terminal, a file, or stderr? Need to capture it and use it for
-- your own means? Now you can, with 'silence' and 'capture'.

module System.IO.Silently (
  silence, hSilence, hSilenceMany,
  capture, hCapture, hCaptureMany
) where

import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (Handle, stdout, hClose, openTempFile, openFile, IOMode)
import Control.Exception (bracket)
import System.Directory (removeFile)

-- | Run an IO action while preventing all output to stdout.
-- This will, as a side effect, create and delete a temp file in the current directory.
silence :: IO a -> IO a
silence = hSilence stdout

-- | Run an IO action while preventing all output to the given handle.
-- This will, as a side effect, create and delete a temp file in the current directory.
hSilence :: Handle -> IO a -> IO a
hSilence handle action = hSilenceMany [handle] action

-- | Run an IO action while preventing all output to the given handles.
-- This will, as a side effect, create and delete a temp file in the current directory.
hSilenceMany :: [Handle] -> IO a -> IO a
hSilenceMany handles action = do
  oldHandles <- mapM hDuplicate handles
  bracket (openFile "/dev/null" AppendMode)
          (\(tmpFile, tmpHandle) -> do sequence_ $ zipWith hDuplicateTo oldHandles handles
                                       hClose tmpHandle
                                       removeFile tmpFile)
          (\(_,       tmpHandle) -> do mapM_ (hDuplicateTo tmpHandle) handles
                                       action)


-- | Run an IO action while preventing and capturing all output to stdout.
-- This will, as a side effect, create and delete a temp file in the current directory.
capture :: IO a -> IO (String, a)
capture = hCapture stdout

-- | Run an IO action while preventing and capturing all output to the given handle.
-- This will, as a side effect, create and delete a temp file in the current directory.
hCapture :: Handle -> IO a -> IO (String, a)
hCapture handle action = hCaptureMany [handle] action


-- | Run an IO action while preventing and capturing all output to the given handles.
-- This will, as a side effect, create and delete a temp file in the current directory.
hCaptureMany :: [Handle] -> IO a -> IO (String, a)
hCaptureMany handles action = do
  oldHandles <- mapM hDuplicate handles
  bracket (openTempFile "." "capture")
          (\(tmpFile, tmpHandle) -> do sequence_ $ zipWith hDuplicateTo oldHandles handles
                                       hClose tmpHandle
                                       removeFile tmpFile)
          (\(tmpFile, tmpHandle) -> do mapM_ (hDuplicateTo tmpHandle) handles
                                       a <- action
                                       hClose tmpHandle
                                       str <- readFile tmpFile
                                       return (str, a))
