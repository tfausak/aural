module Aural.Json
  ( Event(..)
  , decode
  , encode
  ) where

import qualified Data.Char
import qualified Text.ParserCombinators.ReadP
import qualified Text.Printf
import qualified Text.Read

data Event
  = Null
  | Boolean Bool
  | Number Double
  | BeginString
  | Character Char
  | EndString
  | BeginArray
  | ValueSeparator
  | EndArray
  | BeginObject
  | NameSeparator
  | EndObject
  deriving (Eq, Show)

decode :: String -> [Either Char Event]
decode string =
  case string of
    "" -> []
    ' ' : rest -> decode rest
    '\n' : rest -> decode rest
    '\r' : rest -> decode rest
    '\t' : rest -> decode rest
    '"' : rest -> Right BeginString : decodeString rest
    '[' : rest -> Right BeginArray : decode rest
    ',' : rest -> Right ValueSeparator : decode rest
    ']' : rest -> Right EndArray : decode rest
    '{' : rest -> Right BeginObject : decode rest
    ':' : rest -> Right NameSeparator : decode rest
    '}' : rest -> Right EndObject : decode rest
    'n' : 'u' : 'l' : 'l' : rest -> Right Null : decode rest
    't' : 'r' : 'u' : 'e' : rest -> Right (Boolean True) : decode rest
    'f' : 'a' : 'l' : 's' : 'e' : rest -> Right (Boolean False) : decode rest
    _ -> decodeNumber string

decodeString :: String -> [Either Char Event]
decodeString string =
  case string of
    "" -> []
    '"' : rest -> Right EndString : decode rest

    '\\' : rest -> case rest of
      '"' : more -> Right (Character '"') : decodeString more
      '\\' : more -> Right (Character '\\') : decodeString more
      '/' : more -> Right (Character '/') : decodeString more
      'b' : more -> Right (Character '\b') : decodeString more
      'f' : more -> Right (Character '\f') : decodeString more
      'n' : more -> Right (Character '\n') : decodeString more
      'r' : more -> Right (Character '\r') : decodeString more
      't' : more -> Right (Character '\t') : decodeString more

      'u' : a : b : c : d : more -> case fromHex a b c d of
        Just x -> if 0xd800 <= x && x <= 0xdbff
          then case more of
            '\\' : 'u' : e : f : g : h : evenMore -> case fromHex e f g h of
              Just y -> if 0xdc00 <= y && y <= 0xdfff
                then Right (Character (Data.Char.chr (fromPair x y))) : decodeString evenMore
                else Left (Data.Char.chr x) : Right (Character (Data.Char.chr y)) : decodeString evenMore
              Nothing -> Left (Data.Char.chr x) : map Left ['\\', 'u', e, f, g, h] ++ decodeString evenMore
            _ -> Left (Data.Char.chr x) : decodeString more
          else if 0xdc00 <= x && x <= 0xdfff
            then Left (Data.Char.chr x) : decodeString more
            else Right (Character (Data.Char.chr x)) : decodeString more
        Nothing -> map Left ['\\', 'u', a, b, c, d] ++ decodeString more

      "" -> [Left '\\']
      char : more -> Left '\\' : Left char : decodeString more

    char : rest -> if isControl char
      then Left char : decodeString rest
      else Right (Character char) : decodeString rest

fromPair :: Int -> Int -> Int
fromPair hi lo =
  0x10000 + (((hi - 0xd800) * 0x0400) + (lo - 0xdc00))

fromHex :: Char -> Char -> Char -> Char -> Maybe Int
fromHex a b c d = Text.Read.readMaybe ['0', 'x', a, b, c, d]

-- TODO: Decoding numbers (doubleP) is very slow.
decodeNumber :: String -> [Either Char Event]
decodeNumber string = case string of
  "" -> []
  char : rest -> case runReadP doubleP string of
    Nothing -> Left char : decode rest
    Just (double, more) -> Right (Number double) : decode more

runReadP :: Text.ParserCombinators.ReadP.ReadP a -> String -> Maybe (a, String)
runReadP p = safeLast . Text.ParserCombinators.ReadP.readP_to_S p

safeLast :: [a] -> Maybe a
safeLast xs = case xs of
  [] -> Nothing
  [x] -> Just x
  _ : ys -> safeLast ys

doubleP :: Text.ParserCombinators.ReadP.ReadP Double
doubleP = do
  minus <- Text.ParserCombinators.ReadP.option "" $
    Text.ParserCombinators.ReadP.string "-"
  integral <- (Text.ParserCombinators.ReadP.+++)
    (Text.ParserCombinators.ReadP.string "0") $ do
      nonZero <- Text.ParserCombinators.ReadP.satisfy isNonZeroDigit
      digits <- Text.ParserCombinators.ReadP.munch isDigit
      pure (nonZero : digits)
  fractional <- Text.ParserCombinators.ReadP.option "" $ do
    period <- Text.ParserCombinators.ReadP.char '.'
    digits <- Text.ParserCombinators.ReadP.munch1 isDigit
    pure (period : digits)
  power <- Text.ParserCombinators.ReadP.option "" $ do
    e <- Text.ParserCombinators.ReadP.satisfy isE
    sign <- Text.ParserCombinators.ReadP.option '+' $
      Text.ParserCombinators.ReadP.satisfy isSign
    digits <- Text.ParserCombinators.ReadP.munch1 isDigit
    pure (e : sign : digits)
  either fail pure
    . Text.Read.readEither
    $ concat [minus, integral, fractional, power]

isNonZeroDigit :: Char -> Bool
isNonZeroDigit c = '1' <= c && c <= '9'

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

isE :: Char -> Bool
isE c = c == 'e' || c == 'E'

isSign :: Char -> Bool
isSign c = c == '-' || c == '+'

encode :: [Event] -> String
encode = concatMap encodeEvent

encodeEvent :: Event -> String
encodeEvent event =
  case event of
    Null -> "null"
    Boolean boolean -> encodeBoolean boolean
    Number number -> encodeNumber number
    BeginString -> "\""
    Character character -> encodeCharacter character
    EndString -> "\""
    BeginArray -> "["
    ValueSeparator -> ","
    EndArray -> "]"
    BeginObject -> "{"
    NameSeparator -> ":"
    EndObject -> "}"

encodeBoolean :: Bool -> String
encodeBoolean boolean = if boolean then "true" else "false"

-- TODO: Encoding numbers is very slow.
encodeNumber :: Double -> String
encodeNumber number = if isNaN number || isInfinite number
  then "null"
  else Text.Printf.printf "%f" number

encodeCharacter :: Char -> String
encodeCharacter character = case character of
  '"' -> "\\\""
  '\\' -> "\\\\"
  '\b' -> "\\b"
  '\f' -> "\\f"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  _ -> if isControl character
    then Text.Printf.printf "\\u%04x" $ Data.Char.ord character
    else [character]

isControl :: Char -> Bool
isControl = (<= '\x1f')
