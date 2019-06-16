module Main
  ( main
  ) where

import qualified Aural.Either
import qualified Aural.Utf8
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

  T.describe "Utf8" $ do

    T.describe "decode" $ do

      T.it "decodes empty input" $ do
        Aural.Utf8.decode [] `T.shouldBe` []

      T.it "decodes one byte" $ do
        Aural.Utf8.decode [0x24] `T.shouldBe` [Right '\x24']

      T.it "decodes two bytes" $ do
        Aural.Utf8.decode [0xc2, 0xa2] `T.shouldBe` [Right '\xa2']

      T.it "decodes three bytes" $ do
        Aural.Utf8.decode [0xe2, 0x82, 0xac] `T.shouldBe` [Right '\x20ac']

      T.it "decodes four bytes" $ do
        Aural.Utf8.decode [0xf0, 0x90, 0x8d, 0x88] `T.shouldBe` [Right '\x10348']

      -- https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt

      T.it "passes 3.1.9" $ do
        mapM_
          (\ byte -> Aural.Utf8.decode [byte] `T.shouldBe` [Left byte])
          [0x80 .. 0xbf]

      T.it "passes 3.2.1" $ do
        mapM_
          (\ byte -> Aural.Utf8.decode [byte, 0x20] `T.shouldBe` [Left byte, Right '\x20'])
          [0xc0 .. 0xdf]

      T.it "passes 3.2.2" $ do
        mapM_
          (\ byte -> Aural.Utf8.decode [byte, 0x20] `T.shouldBe` [Left byte, Right '\x20'])
          [0xe0 .. 0xef]

      T.it "passes 3.2.3" $ do
        mapM_
          (\ byte -> Aural.Utf8.decode [byte, 0x20] `T.shouldBe` [Left byte, Right '\x20'])
          [0xf0 .. 0xf7]

    T.describe "encode" $ do

      T.it "is" T.pending

  T.describe "Version" $ do

    T.describe "version" $ do
      let Data.Version.Version branch tags = Aural.Version.version

      T.it "has four branches" $ do
        branch `T.shouldSatisfy` ((== 4) . length)

      T.it "starts with one" $ do
        branch `T.shouldSatisfy` Data.List.isPrefixOf [1]

      T.it "has no tags" $ do
        tags `T.shouldSatisfy` null
