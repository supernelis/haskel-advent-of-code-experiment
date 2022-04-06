module Main where

-- Run this test with the command:
-- stack build Testing:test:spec-test

import Test.Hspec

import Basic
import System.IO
import Data.Char

main :: IO ()
main = do
  hspec day3Spec 

gamma_bit column = do
    let nbOnes = sum column
    let nbZeros = (length column) - nbOnes
    if nbOnes > nbZeros 
        then 1
        else 0

gamma_bits columns = do
    map (gamma_bit) columns

rows_to_columns rows = do
    map digitToInt "00100"
 
day3Spec :: Spec
day3Spec = describe "day3" $ do
  describe "gamma_bit" $ do
    it "returns zero when there are more zeros" $ do
      let first_column = [0, 0, 1]
      gamma_bit first_column `shouldBe` 0
    it "returns one when there are more ones" $ do
      let first_column = [1, 0, 1]
      gamma_bit first_column `shouldBe` 1
  describe "gamma for columns" $ do 
    it "calculates the gamma bits" $ do
      let columns = [ [1, 0, 1], [0, 0, 1]]
      gamma_bits columns `shouldBe` [1, 0]
  describe  "transform rows to columns" $ do
    it "something" $ do
        let rows = ["00100"]
        rows_to_columns rows `shouldBe` [0,0,1,0,0]