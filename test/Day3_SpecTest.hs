module Main where

-- Run this test with the command:
-- stack build Testing:test:spec-test

import Test.Hspec

import Basic
import System.IO

main :: IO ()
main = do
  hspec day3Spec 

gamma_bit column = do
    let nbOnes = sum column
    let nbZeros = (length column) - nbOnes
    if nbOnes > nbZeros 
        then 1
        else 0


day3Spec :: Spec
day3Spec = describe "day3" $ do
  describe "gamma_bit" $ do
    it "returns zero when there are more zeros" $ do
      let first_column = [0, 0, 1]
      gamma_bit first_column `shouldBe` 0
    it "returns one when there are more ones" $ do
      let first_column = [1, 0, 1]
      gamma_bit first_column `shouldBe` 1