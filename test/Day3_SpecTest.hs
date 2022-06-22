module Main where

-- Run this test with the command:
-- stack build Testing:test:spec-test

import Test.Hspec

import Basic
import System.IO
import Data.Char
import Data.List

main :: IO ()
main = do
  hspec day3Spec 

gamma_bit column = do
    let nbOnes = sum column
    let nbZeros = (length column) - nbOnes
    if nbOnes > nbZeros 
        then 1
        else 0

gamma_bits_columns columns = do
    map (gamma_bit) columns
    
gamma_bits rows = gamma_bits_columns (rows_to_columns rows)

gamma_rate rows = bitsToInteger (gamma_bits rows)

epsilon_bits rows = bit_flip (gamma_bits rows)

bit_flip bits = map (\x -> abs (x-1)) bits

rows_to_columns rows = do
    let columns = map (map digitToInt) rows
    transpose columns

convert :: [Int] -> Int
convert [] = 0
convert (x : xs) = x + 2 * convert xs

reverseList [] = []
reverseList (x : xs) = (reverse xs) ++ [x] 

bitsToInteger :: [Int] -> Int
bitsToInteger bits = convert (reverseList bits)

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
      gamma_bits_columns columns `shouldBe` [1, 0]
  describe  "transform rows to columns" $ do
    it "transforms rows to columns" $ do
        let rows = ["00100", "11110"]
        rows_to_columns rows `shouldBe` [[0, 1],[0, 1],[1, 1],[0, 1],[0, 0]]
  describe  "gamma" $ do
    it "calculates gamma" $ do
        let rows = ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]
        gamma_bits_columns (rows_to_columns rows) `shouldBe` [1,0,1,1,0]
    it "return gamma as decimal" $ do
       let rows = ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]
       gamma_rate rows `shouldBe` 22
  describe "epsilon" $ do
    it "calculatess epsilon" $ do
      let rows = ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]
      epsilon_bits rows `shouldBe` [0,1,0,0,1]