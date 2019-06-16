module Main
  ( main
  ) where

import qualified Aural.Either
import qualified Test.Hspec as T

main :: IO ()
main = T.hspec . T.describe "Aural" $ do
  T.describe "Either" $ do
    T.describe "unsafeFromRight" $ do
      T.it "with left" $ do
        let l = "oh no"
        Aural.Either.unsafeFromRight (Left l) `T.shouldThrow` T.errorCall l
      T.it "with right" $ do
        Aural.Either.unsafeFromRight (Right ()) `T.shouldBe` ()
