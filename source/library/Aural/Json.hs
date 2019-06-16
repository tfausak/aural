module Aural.Json
  ( Event(..)
  , decode
  , encode
  ) where

import qualified Data.Char
import qualified Text.Printf

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
    -- TODO: handle escapes
    char : rest -> if isControl char
      then Left char : decodeString rest
      else Right (Character char) : decodeString rest

decodeNumber :: String -> [Either Char Event]
decodeNumber string =
  case string of
    "" -> []
    first : rest -> Left first : decode rest -- TODO

encode :: [Event] -> String
encode = concatMap encodeEvent

encodeEvent :: Event -> String
encodeEvent event =
  case event of
    Null -> "null"
    Boolean boolean -> if boolean then "true" else "false"
    Number number -> if isNaN number || isInfinite number
      then "null"
      else Text.Printf.printf "%f" number
    BeginString -> "\""
    Character character -> case character of
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\b' -> "\\b"
      '\f' -> "\\f"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      _ -> if isControl character
        then Text.Printf.printf "\\u%04d" $ Data.Char.ord character
        else [character]
    EndString -> "\""
    BeginArray -> "["
    ValueSeparator -> ","
    EndArray -> "]"
    BeginObject -> "{"
    NameSeparator -> ":"
    EndObject -> "}"

isControl :: Char -> Bool
isControl = (<= '\x1f')
