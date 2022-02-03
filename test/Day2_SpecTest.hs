module Main where

-- Run this test with the command:
-- stack build Testing:test:spec-test

import Test.Hspec

import Basic
import System.IO

main :: IO ()
main = hspec day2Spec

data Position = Position Int
  deriving (Show, Eq)

data Depth = Depth Int
  deriving (Show, Eq)

data Submarine = Submarine {position :: Position, depth :: Depth}
  deriving (Show, Eq)

forward (Position origin) steps = Position (origin + steps)

forward2 (Submarine (Position origin) (Depth d)) steps = Submarine (Position (origin + steps)) (Depth d)

day2Spec :: Spec
day2Spec = describe "day2" $ do
  context "" $ do
    it "should move the submarine forward" $
      forward (Position 0) 5 `shouldBe` Position 5
    it "should move from 5 to 10" $
      forward (Position 5) 5 `shouldBe` Position 10
    it "should move two times forward" $
      forward (forward (Position 0) 5) 5 `shouldBe` Position 10
    it "should move submarine" $
      forward2 (Submarine (Position 0) (Depth 0)) 5 `shouldBe` Submarine (Position 5) (Depth 0)
    it "" $
      forward2 (Submarine (Position 5) (Depth 1)) 5 `shouldBe` Submarine (Position 10) (Depth 1)