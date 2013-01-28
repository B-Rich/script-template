module Main where

import Test.Hspec

import qualified BaseSpec

main :: IO ()
main = hspec $ do
  describe "Base"    BaseSpec.spec
