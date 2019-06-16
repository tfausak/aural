module Aural.Utf8
  ( decode
  , encode
  ) where

import qualified Data.Bits
import qualified Data.Char
import qualified Data.Word

-- | Decodes a stream of bytes as UTF-8 encoded text.
decode :: [Data.Word.Word8] -> [Either Data.Word.Word8 Char]
decode bytes =
  case bytes of
    a : b : c : d : rest | valid4 a b c d -> Right (decode4 a b c d) : decode rest
    a : b : c : rest | valid3 a b c -> Right (decode3 a b c) : decode rest
    a : b : rest | valid2 a b -> Right (decode2 a b) : decode rest
    a : rest | valid1 a -> Right (decode1 a) : decode rest
    a : rest -> Left a : decode rest
    [] -> []

valid1 :: Data.Word.Word8 -> Bool
valid1 a =
  a <= 0x7f

decode1 :: Data.Word.Word8 -> Char
decode1 a =
  Data.Char.chr $
    decodeHelper 0x7f 0 a

valid2 :: Data.Word.Word8 -> Data.Word.Word8 -> Bool
valid2 a b =
  0xc2 <= a && a <= 0xdf &&
  0x80 <= b && b <= 0xbf

decode2 :: Data.Word.Word8 -> Data.Word.Word8 -> Char
decode2 a b =
  Data.Char.chr $
    decodeHelper 0x1f 6 a Data.Bits..|.
    decodeHelper 0x3f 0 b

valid3 :: Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8 -> Bool
valid3 a b c =
  0xe0 <= a && a <= 0xef &&
  0x80 <= b && b <= 0xbf &&
  0x80 <= c && c <= 0xbf

decode3 :: Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8 -> Char
decode3 a b c =
  Data.Char.chr $
    decodeHelper 0x0f 12 a Data.Bits..|.
    decodeHelper 0x3f 6 b Data.Bits..|.
    decodeHelper 0x3f 0 c

valid4 :: Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8 -> Bool
valid4 a b c d =
  0xf0 <= a && a <= 0xf5 &&
  0x80 <= b && b <= 0xbf &&
  0x80 <= c && c <= 0xbf &&
  0x80 <= d && d <= 0xbf

decode4 :: Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8 -> Char
decode4 a b c d =
  Data.Char.chr $
    decodeHelper 0x07 18 a Data.Bits..|.
    decodeHelper 0x3f 12 b Data.Bits..|.
    decodeHelper 0x3f 6 c Data.Bits..|.
    decodeHelper 0x3f 0 d

decodeHelper :: Data.Word.Word8 -> Int -> Data.Word.Word8 -> Int
decodeHelper mask shift word8 =
  let
    (.&) :: Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8
    (.&) = (Data.Bits..&.)

    word8ToInt :: Data.Word.Word8 -> Int
    word8ToInt = fromIntegral

    (.<<) :: Int -> Int -> Int
    (.<<) = Data.Bits.shiftL
  in
    word8ToInt (word8 .& mask) .<< shift

-- | Encodes text as a stream of UTF-8 bytes.
encode :: String -> [Data.Word.Word8]
encode = concatMap (encodeInt . Data.Char.ord)

encodeInt :: Int -> [Data.Word.Word8]
encodeInt x
  | x <= 0x7f =
    [ encodeHelper 0 0x7f 0x00 x
    ]
  | x <= 0x7ff =
    [ encodeHelper 6 0x3f 0xc0 x
    , encodeHelper 0 0x3f 0x80 x
    ]
  | x <= 0xffff =
    [ encodeHelper 12 0x0f 0xe0 x
    , encodeHelper 6 0x3f 0x80 x
    , encodeHelper 0 0x3f 0x80 x
    ]
  | otherwise =
    [ encodeHelper 18 0x07 0xf0 x
    , encodeHelper 12 0x3f 0x80 x
    , encodeHelper 6 0x3f 0x80 x
    , encodeHelper 0 0x3f 0x80 x
    ]

encodeHelper :: Int -> Int -> Data.Word.Word8 -> Int -> Data.Word.Word8
encodeHelper shift mask label int =
  let
    (.>>) :: Int -> Int -> Int
    (.>>) = Data.Bits.shiftR

    (.&) :: Int -> Int -> Int
    (.&) = (Data.Bits..&.)

    intToWord8 :: Int -> Data.Word.Word8
    intToWord8 = fromIntegral

    (.|) :: Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8
    (.|) = (Data.Bits..|.)
  in
    label .| intToWord8 ((int .>> shift) .& mask)
