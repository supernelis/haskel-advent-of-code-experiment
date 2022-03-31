module Main where

-- Run this test with the command:
-- stack build Testing:test:spec-test

import Test.Hspec

import Basic
import System.IO

main :: IO ()
main = do
  hspec day2Spec 
  hspec day2Pt2Spec 


data Position = Position Int
  deriving (Show, Eq)

data Depth = Depth Int
  deriving (Show, Eq)

data Aim = Aim Int
  deriving (Show, Eq)

data Sub = Submarine Position Depth | ComplexSubmarine Position Depth Aim deriving (Show, Eq)

multiply (Submarine (Position p) (Depth d)) = p * d

forward (Submarine (Position origin) (Depth d)) steps = Submarine (Position (origin + steps)) (Depth d)
forward (ComplexSubmarine (Position origin) (Depth d) (Aim aim)) steps = ComplexSubmarine (Position (origin+steps)) (Depth (d+(aim * steps))) (Aim aim)

down (Submarine (Position origin) (Depth d)) steps = Submarine (Position origin) (Depth (d+steps))
down (ComplexSubmarine (Position origin) (Depth d) (Aim aim)) steps = ComplexSubmarine (Position origin) (Depth d) (Aim (aim+steps))

up (Submarine (Position origin) (Depth d)) steps = Submarine (Position origin) (Depth (d-steps))
up (ComplexSubmarine (Position origin) (Depth d) (Aim aim)) steps = ComplexSubmarine (Position origin) (Depth d) (Aim (aim-steps))

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

executeInstructions submarine instructions = do
  foldl (\acc x-> execute x acc) submarine instructions

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
    it "should execute two instructions" $ do
      let instructions = [Instruction Forward 3,Instruction Down 3]
      executeInstructions (Submarine (Position 0) (Depth 0)) instructions `shouldBe` Submarine (Position 3) (Depth 3)
    it "should executeInstructions" $ do
      let lines = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]
      let instructions = map (parseInstruction) lines
      executeInstructions (Submarine (Position 0) (Depth 0)) instructions `shouldBe` Submarine (Position 15) (Depth 10)
    it "should multiply position by depth" $ do
      multiply (Submarine (Position 15) (Depth 10)) `shouldBe` 150
    it "solves the first part of the puzzle" $ do
      instructions <- parseInputFile
      multiply (executeInstructions (Submarine (Position 0) (Depth 0)) instructions) `shouldBe` 2150351

day2Pt2Spec :: Spec
day2Pt2Spec = describe "day2 part 2" $ do
  describe "commands" $ do
    it "should execute the down instruction using aim" $ do
      down (ComplexSubmarine (Position 0) (Depth 5) (Aim 0)) 5 `shouldBe` ComplexSubmarine (Position 0) (Depth 5) (Aim 5)
    it "should execute the up instruction using aim" $ do
      up (ComplexSubmarine (Position 0) (Depth 5) (Aim 0)) 5 `shouldBe` ComplexSubmarine (Position 0) (Depth 5) (Aim (-5))
    it "should execute the forward instruction using aim" $ do
      forward (ComplexSubmarine (Position 0) (Depth 0) (Aim 2)) 2 `shouldBe` ComplexSubmarine (Position 2) (Depth 4) (Aim 2)
