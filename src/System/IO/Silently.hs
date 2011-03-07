
module System.IO.Silently (
  silently, hSilently
) where

import GHC.IO.Handle
import System.IO
import Control.Exception (finally)
import System.Directory

-- | Run an IO action while ignoring all output to stdout.
silently :: IO a -> IO a
silently = hSilently stdout

-- | Run an IO action while ignoring all output to the given handle.
hSilently :: Handle -> IO a -> IO a
hSilently handle action = do
  oldHandle <- hDuplicate handle
  (tmpFile, tmpHandle) <- openTempFile "." "silently"
  hDuplicateTo tmpHandle handle
  finally action (hDuplicateTo oldHandle handle >> hClose tmpHandle >> removeFile tmpFile)
