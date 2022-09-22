{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module System.IO.Silently.WinIO where

import Control.Exception (bracket)
import Data.Bits ((.|.))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (poke, sizeOf)
import Graphics.Win32.Misc
import System.IO (Handle, hGetBuffering, hSetBuffering, stderr, stdin, stdout)
import System.Win32.File
    ( SECURITY_ATTRIBUTES (..)
    , createFile
    , fILE_SHARE_READ
    , fILE_SHARE_WRITE
    , gENERIC_READ
    , gENERIC_WRITE
    , oPEN_EXISTING
    )
import System.Win32.Types (BOOL, HANDLE, failIfFalse_, withHandleToHANDLE)

foreign import capi unsafe "windows.h SetStdHandle"
  c_SetStdHandle :: StdHandleId -> HANDLE -> IO BOOL

setStdHandle :: StdHandleId -> HANDLE -> IO ()
setStdHandle hId handle = failIfFalse_ "SetStdHandle" (c_SetStdHandle hId handle)

openConsoleHandle :: Bool -> IO HANDLE
openConsoleHandle isRead = alloca $ \lpSecurityAttr -> do
  poke lpSecurityAttr securityAttr
  createFile
    (if isRead then "CONIN$" else "CONOUT$")
    (gENERIC_READ .|. gENERIC_WRITE)
    (fILE_SHARE_READ .|. fILE_SHARE_WRITE)
    (Just lpSecurityAttr)
    oPEN_EXISTING
    0 -- ignored
    Nothing -- ignored
  where securityAttr = SECURITY_ATTRIBUTES
          { nLength = fromIntegral (sizeOf (undefined :: SECURITY_ATTRIBUTES))
          , lpSecurityDescriptor = nullPtr
          , bInheritHandle = True
          }

handleId :: Handle -> Maybe (StdHandleId, Bool)
handleId h
  | h == stdin  = Just (sTD_INPUT_HANDLE,  True)
  | h == stdout = Just (sTD_OUTPUT_HANDLE, False)
  | h == stderr = Just (sTD_ERROR_HANDLE,  False)
  | otherwise   = Nothing

goBracket :: ([Handle] -> IO a) -> Handle -> Handle -> [Handle] -> IO a
goBracket go tmpHandle h hs = case handleId h of
  Just (hId, isInput) -> do
    buffering <- hGetBuffering h
    withHandleToHANDLE tmpHandle $ \tmpHANDLE -> do
      bracket
        (setStdHandle hId tmpHANDLE)
        (\_ -> do
          stdHandle <- openConsoleHandle isInput
          setStdHandle hId stdHandle
          hSetBuffering h buffering)
        (\_ -> go hs)
  -- unknown handle, but there is nothing useful we could do
  -- deliberately not producing any error message
  Nothing -> go hs
