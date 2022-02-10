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

data Command = Forward | Up | Down
  deriving (Show, Eq)

data Instruction = Instruction {command :: Command, value :: Int}
  deriving (Show, Eq)

parseCommand "forward" = Forward
parseCommand "down" = Down
parseCommand "up" = Up

parseInstruction instruction = do
  let (command:value:[]) = words instruction
  Instruction (parseCommand command) (read value)

readInputFile = do
  rawcontent <- readFile "test/input_day2"
  return (lines rawcontent)

parseInputFile = do
  raw_lines <- readInputFile
  return (map parseInstruction raw_lines)

execute (Instruction Forward value) submarine = forward submarine value
execute (Instruction Down value) submarine = down submarine value
execute (Instruction Up value) submarine = up submarine value

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
  describe "parse instruction" $ do
    it "should parse a forward instruction" $
      parseInstruction "forward 3" `shouldBe` Instruction Forward 3
    it "should parse a down instruction" $
      parseInstruction "down 5" `shouldBe` Instruction Down 5
  describe "read file" $ do
    it "should return the first line" $ do
      lines <- readInputFile 
      head lines `shouldBe` "forward 3"
  describe "parse file" $ do
    it "should return the instructions" $ do
      instructions <- parseInputFile
      head instructions `shouldBe` Instruction Forward 3
  describe "execute instruction" $ do
    it "should execute the instruction" $ do
      let instruction = Instruction Forward 3
      let submarine = Submarine (Position 0) (Depth 0)
      execute instruction submarine `shouldBe` Submarine (Position 3) (Depth 0)
    it "should execute the down instruction" $ do
      let instruction = Instruction Down 3
      let submarine = Submarine (Position 0) (Depth 0)
      execute instruction submarine `shouldBe` Submarine (Position 0) (Depth 3)
    it "should execute the up instruction" $ do
      let instruction = Instruction Up 3
      let submarine = Submarine (Position 0) (Depth 3)
      execute instruction submarine `shouldBe` Submarine (Position 0) (Depth 0)
    it "should fold" $ do
      foldl (++) "c" ["a","b"] `shouldBe` "cab"
    it "should execute two instructions" $ do
      let submarine = Submarine (Position 0) (Depth 0)
      let instructions = [Instruction Forward 3,Instruction Down 3]
      foldl (\acc x-> execute x acc) submarine instructions `shouldBe` Submarine (Position 3) (Depth 3)