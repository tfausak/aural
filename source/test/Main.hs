module Main
  ( main
  ) where

import qualified Aural.Either
import qualified Aural.Version
import qualified Data.List
import qualified Data.Version
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

  T.describe "Version" $ do

    T.describe "version" $ do
      let Data.Version.Version branch tags = Aural.Version.version

      T.it "has four branches" $ do
        branch `T.shouldSatisfy` ((== 4) . length)

      T.it "starts with one" $ do
        branch `T.shouldSatisfy` Data.List.isPrefixOf [1]

      T.it "has no tags" $ do
        tags `T.shouldSatisfy` null
