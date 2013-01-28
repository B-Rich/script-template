module BaseSpec where

import Test.Hspec (Spec, describe, it, hspec)
import Test.Hspec.HUnit ()
import Test.Hspec.Expectations

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "returns the first element of a list" $ do
            head [23 ..] `shouldBe` (23 :: Int)

-- BaseSpec.hs ends here
