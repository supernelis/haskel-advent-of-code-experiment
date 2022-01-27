module Main where

-- Run this test with the command:
-- stack build Testing:test:spec-test

import Test.Hspec

import Basic
import System.IO

main :: IO ()
main = hspec day1Spec

parseInputFile = do
  raw_lines <- readInputFile
  return (map read raw_lines)

readInputFile = do
  rawcontent <- readFile "test/input_day1"
  return (lines rawcontent)

count (x:xs:[]) = 
  if x < xs
    then 1
    else 0

count (x:rest) = (count [x, (head rest)]) + (count rest)

sumOfWindows (x:y:z:[]) = [x + y + z]

sumOfWindows (x:y:z:rest) = sumOfWindows [x,y,z] ++ sumOfWindows ([y,z] ++ rest)

countIncreasesInSlidingWindow (measurements) = count (sumOfWindows measurements)

day1Spec :: Spec
day1Spec = describe "day1" $ do
  context "read input file" $ do
    it "should return the first line" $ do
      lines <- readInputFile 
      head lines `shouldBe` "191"
    it "converts" $ do
      lines <- readInputFile
      read (head lines) `shouldBe` 191
    it "maps all lines to integer" $ do
      lines <- parseInputFile
      head lines `shouldBe` 191
  context "count increases for two values" $
    it "returns 1 increasing" $ do
      count([199, 200]) `shouldBe` 1
      count([199, 198]) `shouldBe` 0
  context "count increases for three values" $
    it "ddd" $ do
      count([1, 2, 3]) `shouldBe` 2
  context "count increase for example" $
    it "counts" $
      count([199,200,208,210,200,207,240,269,260,263]) `shouldBe` 7
  context "count increases for puzzle" $
    it "counts" $ do
      lines <- parseInputFile
      count lines `shouldBe` 1709
  context "sumOfWindows" $ do
    it "should sum the elements in the sliding window" $
      sumOfWindows([199,200,208]) `shouldBe` [607]
    it "should calculate the sum of two sliding windows" $
      sumOfWindows([199,200,208,210]) `shouldBe` [607, 618]
    it "should calculate the sum of three sliding windows" $
      sumOfWindows([199,200,208,210,300]) `shouldBe` [607,618,718]
  context "countIncreasesInSlidingWindow" $ do
    it "counts increases between 3 sliding windows" $
      countIncreasesInSlidingWindow([199,200,208,210,300]) `shouldBe` 2
    it "counts increases between sliding windows in example" $
      countIncreasesInSlidingWindow([199,200,208,210,200,207,240,269,260,263]) `shouldBe` 5
    it "solve second puzzle day 1" $ do
      lines <- parseInputFile
      countIncreasesInSlidingWindow lines `shouldBe` 1761
