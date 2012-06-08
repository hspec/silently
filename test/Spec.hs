module Main (main) where

import           Test.Hspec.ShouldBe
import           System.IO.Silently

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do

  describe "capture" $ do
    it "captures stdout" $ do
      capture (putStr "foo" >> return 23) `shouldReturn` ("foo", 23)
