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

count (x:xs) = 
  if x < head xs
    then 1
    else 0
  
day1Spec :: Spec
day1Spec = describe "day1" $ do
  context "read input file" $
    it "should return the first line" $ do
      lines <- readInputFile 
      head lines `shouldBe` "191" 
  context "count increases" $
    it "something" $ do
      count([199, 200]) `shouldBe` 1
      count([199, 198]) `shouldBe` 0