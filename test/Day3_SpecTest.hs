module Main where

-- Run this test with the command:
-- stack build Testing:test:spec-test

import Test.Hspec

import Basic
import System.IO

main :: IO ()
main = do
  hspec day3Spec 

gamma column = do
    let nbOnes = sum column
    let columnLength = length column
    let nbZeros = columnLength - nbOnes
    if nbOnes > nbZeros 
        then 1
        else 0


day3Spec :: Spec
day3Spec = describe "day3" $ do
  describe "something" $ do
    it "gamma" $ do
      let first_column = [0, 0, 1]
      gamma first_column `shouldBe` 0
    it "sums list" $ do
      let first_column = [1, 0, 1]
      gamma first_column `shouldBe` 1