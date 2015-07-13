module Main (main) where

import           Test.Hspec
import           System.IO
import           System.IO.Silently
import           System.Directory
import           System.IO.Temp

import           Control.Exception
import           Control.Monad

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "hSilence" $ do
    it "prevents output to a given handle" $ let file = "foo.txt" in do
      h <- openFile file ReadWriteMode
      hSilence [h] $ do
        hPutStrLn h "foo bar baz"
        hFlush h
      hSeek h AbsoluteSeek 0
      hGetContents h `shouldReturn` ""
      `finally` removeFile file

  describe "capture" $ do
    it "captures stdout" $ do
      capture (putStr "foo" >> return 23) `shouldReturn` ("foo", 23 :: Int)

  describe "hCapture" $ do
    forM_ [NoBuffering, LineBuffering, BlockBuffering Nothing] $ \buffering -> do
      it ("preserves " ++ show buffering) $ do
        withSystemTempFile "silently" $ \_file h -> do
          hSetBuffering h buffering
          _ <- hCapture [h] (return ())
          hGetBuffering h `shouldReturn` buffering
