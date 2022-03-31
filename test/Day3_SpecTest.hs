module Main where

-- Run this test with the command:
-- stack build Testing:test:spec-test

import Test.Hspec

import Basic
import System.IO

main :: IO ()
main = do
  hspec day3Spec 

gamma column = sum column


day3Spec :: Spec
day3Spec = describe "day3" $ do
  describe "something" $ do
    it "gamma" $ do
      let first_column = [0, 0]
      gamma first_column `shouldBe` 0