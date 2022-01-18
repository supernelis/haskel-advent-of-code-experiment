module Main where

-- Run this test with the command:
-- stack build Testing:test:spec-test

import Test.Hspec

import Basic
import System.IO

main :: IO ()
main = hspec day1Spec

readInputFile = do
  rawcontent <- readFile "test/input_day1"
  return (lines rawcontent)

count (x:xs:[]) = 
  if x < xs
    then 1
    else 0

count (x:rest) = (count [x, (head rest)]) + (count rest)
  
day1Spec :: Spec
day1Spec = describe "day1" $ do
  context "read input file" $
    it "should return the first line" $ do
      lines <- readInputFile 
      head lines `shouldBe` "191" 
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
      lines <- readInputFile
      read (head lines) `shouldBe` 191
      