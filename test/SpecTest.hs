module Main where

-- Run this test with the command:
-- stack build Testing:test:spec-test

import Test.Hspec

import Basic

main :: IO ()
main = hspec day1Spec

-- TODO: Add more cases!
simpleMathSpec :: Spec
simpleMathSpec = describe "Tests of our simple math function" $ do
  context "when the numbers are small" $
    it "Should match the our expected value" $
      simpleMathFunction 3 4 5 `shouldBe` 7

readInputFile = [191]

day1Spec :: Spec
day1Spec = describe "foo" $ do
  context "blah" $
    it "should do something" $
      head readInputFile `shouldBe` 191