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
decode _ = [] -- TODO

encode :: [Event] -> String
encode = concatMap encodeEvent

encodeEvent :: Event -> String
encodeEvent event =
  case event of
    Null -> "null"
    Boolean boolean -> if boolean then "true" else "false"
    Number number -> Text.Printf.printf "%f" number
    BeginString -> "\""
    Character character -> if character <= '\x1f'
      then Text.Printf.printf "\\u%04d" (Data.Char.ord character)
      else [character]
    EndString -> "\""
    BeginArray -> "["
    ValueSeparator -> ","
    EndArray -> "]"
    BeginObject -> "{"
    NameSeparator -> ":"
    EndObject -> "}"
