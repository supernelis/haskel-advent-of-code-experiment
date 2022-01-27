module Main where

-- Run this test with the command:
-- stack build Testing:test:spec-test

import Test.Hspec

import Basic
import System.IO

main :: IO ()
main = hspec day2Spec

data HPosition = Position Int
  deriving (Show, Eq)

forward (Position origin) steps = Position (origin + steps)

day2Spec :: Spec
day2Spec = describe "day2" $ do
  context "" $ do
    it "should move the submarine forward" $
      forward (Position 0) 5 `shouldBe` Position 5
    it "should move from 5 to 10" $
      forward (Position 5) 5 `shouldBe` Position 10
  