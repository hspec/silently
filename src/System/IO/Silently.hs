
module System.IO.Silently (
  silently, hSilently
) where

import GHC.IO.Handle
import System.IO
import Control.Exception (bracket)
import System.Directory

-- | Run an IO action while ignoring all output to stdout.
silently :: IO a -> IO a
silently = hSilently stdout

-- | Run an IO action while ignoring all output to the given handle.
hSilently :: Handle -> IO a -> IO a
hSilently handle action = do
  oldHandle <- hDuplicate handle
  bracket (openTempFile "." "silently")
          (\(tmpFile, tmpHandle) -> do hDuplicateTo oldHandle handle
                                       hClose tmpHandle
                                       removeFile tmpFile)
          (\(_,       tmpHandle) -> do hDuplicateTo tmpHandle handle
                                       action)
