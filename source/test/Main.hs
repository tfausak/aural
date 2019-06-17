module Main
  ( main
  ) where

import qualified Aural.Either
import qualified Aural.Json
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

  T.describe "Json" $ do

    T.describe "decode" $ do

      T.it "decodes empty input" $ do
        Aural.Json.decode "" `T.shouldBe` []

      T.it "decodes blank space" $ do
        Aural.Json.decode " \n\r\t" `T.shouldBe` []

      T.it "decodes null" $ do
        Aural.Json.decode "null" `T.shouldBe` [Right Aural.Json.Null]

      T.it "decodes booleans" $ do
        Aural.Json.decode "false" `T.shouldBe` [Right $ Aural.Json.Boolean False]
        Aural.Json.decode "true" `T.shouldBe` [Right $ Aural.Json.Boolean True]

      T.it "decodes numbers" $ do
        Aural.Json.decode "0" `T.shouldBe` [Right $ Aural.Json.Number 0]
        Aural.Json.decode "1" `T.shouldBe` [Right $ Aural.Json.Number 1]
        Aural.Json.decode "-1" `T.shouldBe` [Right . Aural.Json.Number $ -1]
        Aural.Json.decode "0.0" `T.shouldBe` [Right $ Aural.Json.Number 0]
        Aural.Json.decode "0.1" `T.shouldBe` [Right $ Aural.Json.Number 0.1]
        Aural.Json.decode "1e0" `T.shouldBe` [Right $ Aural.Json.Number 1]
        Aural.Json.decode "1e+0" `T.shouldBe` [Right $ Aural.Json.Number 1]
        Aural.Json.decode "1E0" `T.shouldBe` [Right $ Aural.Json.Number 1]
        Aural.Json.decode "1.0e0" `T.shouldBe` [Right $ Aural.Json.Number 1]
        Aural.Json.decode "1e9" `T.shouldBe` [Right $ Aural.Json.Number 1e9]
        Aural.Json.decode "1e-9" `T.shouldBe` [Right $ Aural.Json.Number 1e-9]

      T.it "decodes strings" $ do
        Aural.Json.decode "\"\"" `T.shouldBe` map Right (jsonString "")
        Aural.Json.decode "\"ab\"" `T.shouldBe` map Right (jsonString "ab")
        T.pending
        -- Aural.Json.decode "\"\\u0000\"" `T.shouldBe` map Right (jsonString "\x00")
        -- Aural.Json.decode "\"\x10348\"" `T.shouldBe` map Right (jsonString "\x10348")
        -- Aural.Json.decode "\"\\ud834\\udd1e\"" `T.shouldBe` map Right (jsonString "\x1d11e")
        -- Aural.Json.decode "\"\\\"\\\\\\b\\f\\n\\r\\t\"" `T.shouldBe` map Right (jsonString "\"\\\b\f\n\r\t")

      T.it "decodes arrays" $ do
        Aural.Json.decode "[]" `T.shouldBe` map Right (jsonArray [])
        Aural.Json.decode "[1]" `T.shouldBe` map Right (jsonArray [1])
        Aural.Json.decode "[1,2]" `T.shouldBe` map Right (jsonArray [1, 2])

      T.it "decodes objects" $ do
        Aural.Json.decode "{}" `T.shouldBe` map Right (jsonObject [])
        Aural.Json.decode "{\"a\":1}" `T.shouldBe` map Right (jsonObject [('a', 1)])
        Aural.Json.decode "{\"a\":1,\"b\":2}" `T.shouldBe` map Right (jsonObject [('a', 1), ('b', 2)])

    T.describe "encode" $ do

      T.it "encodes empty input" $ do
        Aural.Json.encode [] `T.shouldBe` ""

      T.it "encodes null" $ do
        Aural.Json.encode [Aural.Json.Null] `T.shouldBe` "null"

      T.it "encodes booleans" $ do
        Aural.Json.encode [Aural.Json.Boolean False] `T.shouldBe` "false"
        Aural.Json.encode [Aural.Json.Boolean True] `T.shouldBe` "true"

      T.it "encodes numbers" $ do
        Aural.Json.encode [Aural.Json.Number 0] `T.shouldBe` "0.0"
        Aural.Json.encode [Aural.Json.Number 1e-9] `T.shouldBe` "0.000000001"
        Aural.Json.encode [Aural.Json.Number 1e9] `T.shouldBe` "1000000000.0"
        Aural.Json.encode [Aural.Json.Number (0 / 0)] `T.shouldBe` "null"
        Aural.Json.encode [Aural.Json.Number (1 / 0)] `T.shouldBe` "null"
        Aural.Json.encode [Aural.Json.Number (-1 / 0)] `T.shouldBe` "null"

      T.it "encodes strings" $ do
        let
        Aural.Json.encode (jsonString "") `T.shouldBe` "\"\""
        Aural.Json.encode (jsonString "ab") `T.shouldBe` "\"ab\""
        Aural.Json.encode (jsonString "\x00") `T.shouldBe` "\"\\u0000\""
        Aural.Json.encode (jsonString "\x10348") `T.shouldBe` "\"\x10348\""
        Aural.Json.encode (jsonString "\"\\\b\f\n\r\t") `T.shouldBe` "\"\\\"\\\\\\b\\f\\n\\r\\t\""

      T.it "encodes arrays" $ do
        Aural.Json.encode (jsonArray []) `T.shouldBe` "[]"
        Aural.Json.encode (jsonArray [1]) `T.shouldBe` "[1.0]"
        Aural.Json.encode (jsonArray [1, 2]) `T.shouldBe` "[1.0,2.0]"

      T.it "encodes objects" $ do
        Aural.Json.encode (jsonObject []) `T.shouldBe` "{}"
        Aural.Json.encode (jsonObject [('a', 1)]) `T.shouldBe` "{\"a\":1.0}"
        Aural.Json.encode (jsonObject [('a', 1), ('b', 2)]) `T.shouldBe` "{\"a\":1.0,\"b\":2.0}"

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

    T.describe "encode" $ do

      T.it "encodes empty input" $ do
        Aural.Utf8.encode [] `T.shouldBe` []

      T.it "encodes one byte" $ do
        Aural.Utf8.encode ['\x24'] `T.shouldBe` [0x24]

      T.it "encodes two bytes" $ do
        Aural.Utf8.encode ['\xa2'] `T.shouldBe` [0xc2, 0xa2]

      T.it "encodes three bytes" $ do
        Aural.Utf8.encode ['\x20ac'] `T.shouldBe` [0xe2, 0x82, 0xac]

      T.it "encodes four bytes" $ do
        Aural.Utf8.encode ['\x10348'] `T.shouldBe` [0xf0, 0x90, 0x8d, 0x88]

  T.describe "Version" $ do

    T.describe "version" $ do
      let Data.Version.Version branch tags = Aural.Version.version

      T.it "has four branches" $ do
        branch `T.shouldSatisfy` ((== 4) . length)

      T.it "starts with one" $ do
        branch `T.shouldSatisfy` Data.List.isPrefixOf [1]

      T.it "has no tags" $ do
        tags `T.shouldSatisfy` null

jsonString :: String -> [Aural.Json.Event]
jsonString
  = (Aural.Json.BeginString :)
  . foldr (:) [Aural.Json.EndString]
  . map Aural.Json.Character

jsonArray :: [Double] -> [Aural.Json.Event]
jsonArray
  = (Aural.Json.BeginArray :)
  . foldr (:) [Aural.Json.EndArray]
  . Data.List.intersperse Aural.Json.ValueSeparator
  . map Aural.Json.Number

jsonObject :: [(Char, Double)] -> [Aural.Json.Event]
jsonObject
  = (Aural.Json.BeginObject :)
  . foldr (:) [Aural.Json.EndObject]
  . Data.List.intercalate [Aural.Json.ValueSeparator]
  . map (\ (k, v) ->
    jsonString [k] ++ [Aural.Json.NameSeparator, Aural.Json.Number v])
