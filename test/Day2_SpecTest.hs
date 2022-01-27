module Main where

-- Run this test with the command:
-- stack build Testing:test:spec-test

import Test.Hspec

import Basic
import System.IO

main :: IO ()
main = hspec day2Spec

forward n = n

day2Spec :: Spec
day2Spec = describe "day2" $ do
  context "" $
    it "should move the submarine forward" $
      forward 5 `shouldBe` 5
  