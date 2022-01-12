module Main where

-- Run this test with the command:
-- stack build Testing:test:spec-test

import Test.Hspec

import Basic
import System.IO

main :: IO ()
main = hspec day1Spec

-- TODO: Add more cases!
simpleMathSpec :: Spec
simpleMathSpec = describe "Tests of our simple math function" $ do
  context "when the numbers are small" $
    it "Should match the our expected value" $
      simpleMathFunction 3 4 5 `shouldBe` 7

readInputFile = do
  rawcontent <- readFile "test/input_day1"
  return (lines rawcontent)
  -- content <- (readFile "input_day1")
  


day1Spec :: Spec
day1Spec = describe "foo" $ do
  context "blah" $
    it "should do something" $ do
      lines <- readInputFile 
      head lines `shouldBe` "191"