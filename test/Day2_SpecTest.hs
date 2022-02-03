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

forward (Submarine (Position origin) (Depth d)) steps = Submarine (Position (origin + steps)) (Depth d)

down (Submarine (Position origin) (Depth d)) steps = Submarine (Position origin) (Depth (d+steps))

up (Submarine (Position origin) (Depth d)) steps = Submarine (Position origin) (Depth (d-steps))

day2Spec :: Spec
day2Spec = describe "day2" $ do
  describe "forward function" $ do
    it "should move two times forward" $
      forward (forward (Submarine (Position 5) (Depth 1)) 5) 5 `shouldBe` Submarine (Position 15) (Depth 1)
  describe "down function" $ do
    it "should move two times down" $
      down (down (Submarine (Position 0) (Depth 5)) 5) 5 `shouldBe` Submarine (Position 0) (Depth 15)
  describe "up function" $ do
    it "should move two times up" $
      up (up (Submarine (Position 0) (Depth 15)) 5) 5 `shouldBe` Submarine (Position 0) (Depth 5)