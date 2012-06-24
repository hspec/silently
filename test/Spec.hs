{-# LANGUAGE CPP #-}
module Main (main) where

#ifdef USE_NANOSPEC
import           Test.Hspec.Expectations
import           NanoSpec
#else
import           Test.Hspec.ShouldBe
#endif

import           System.IO
import           System.IO.Silently
import           System.Directory

import           Control.Exception

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "hSilence" $ do
    it "prevents output to a given handle" $ let file = "test/foo.txt" in do
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
