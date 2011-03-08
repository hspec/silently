
-- | Need to prevent output to the terminal or stderr? Need to capture it and use it for your own means? Now you can, with 'silently' and 'capture'.

module System.IO.Silently (
  silently, hSilently, capture, hCapture
) where

import GHC.IO.Handle
import System.IO
import Control.Exception (bracket)
import System.Directory

-- | Run an IO action while preventing all output to stdout.
-- This will, as a side effect, create and delete a temp file in the current directory.
silently :: IO a -> IO a
silently = hSilently stdout

-- | Run an IO action while preventing all output to the given handle.
-- This will, as a side effect, create and delete a temp file in the current directory.
hSilently :: Handle -> IO a -> IO a
hSilently handle action = do
  oldHandle <- hDuplicate handle
  bracket (openTempFile "." "silently")
          (\(tmpFile, tmpHandle) -> do hDuplicateTo oldHandle handle
                                       hClose tmpHandle
                                       removeFile tmpFile)
          (\(_,       tmpHandle) -> do hDuplicateTo tmpHandle handle
                                       action)

-- | Run an IO action while preventing and capturing all output to stdout.
-- This will, as a side effect, create and delete a temp file in the current directory.
capture :: IO a -> IO (String, a)
capture = hCapture stdout

-- | Run an IO action while preventing and capturing all output to the given handle.
-- This will, as a side effect, create and delete a temp file in the current directory.
hCapture :: Handle -> IO a -> IO (String, a)
hCapture handle action = do
  oldHandle <- hDuplicate handle
  bracket (openTempFile "." "silently")
          (\(tmpFile, tmpHandle) -> do hDuplicateTo oldHandle handle
                                       hClose tmpHandle
                                       removeFile tmpFile)
          (\(tmpFile, tmpHandle) -> do hDuplicateTo tmpHandle handle
                                       a <- action
                                       hClose tmpHandle
                                       str <- readFile tmpFile
                                       return (str, a))
