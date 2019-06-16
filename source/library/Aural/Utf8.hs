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
    maskAndShift 0xff 0 a

valid2 :: Data.Word.Word8 -> Data.Word.Word8 -> Bool
valid2 a b =
  0xc2 <= a && a <= 0xdf &&
  0x80 <= b && b <= 0xbf

decode2 :: Data.Word.Word8 -> Data.Word.Word8 -> Char
decode2 a b =
  Data.Char.chr $
    maskAndShift 0x1f 6 a Data.Bits..|.
    maskAndShift 0x3f 0 b

valid3 :: Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8 -> Bool
valid3 a b c =
  0xe0 <= a && a <= 0xef &&
  0x80 <= b && b <= 0xbf &&
  0x80 <= c && c <= 0xbf

decode3 :: Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8 -> Char
decode3 a b c =
  Data.Char.chr $
    maskAndShift 0x0f 12 a Data.Bits..|.
    maskAndShift 0x3f 6 b Data.Bits..|.
    maskAndShift 0x3f 0 c

valid4 :: Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8 -> Bool
valid4 a b c d =
  0xf0 <= a && a <= 0xf5 &&
  0x80 <= b && b <= 0xbf &&
  0x80 <= c && c <= 0xbf &&
  0x80 <= d && d <= 0xbf

decode4 :: Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8 -> Data.Word.Word8 -> Char
decode4 a b c d =
  Data.Char.chr $
    maskAndShift 0x07 18 a Data.Bits..|.
    maskAndShift 0x3f 12 b Data.Bits..|.
    maskAndShift 0x3f 6 c Data.Bits..|.
    maskAndShift 0x3f 0 d

maskAndShift :: Data.Word.Word8 -> Int -> Data.Word.Word8 -> Int
maskAndShift mask shift word8 =
  Data.Bits.shiftL (fromIntegral (word8 Data.Bits..&. mask)) shift

encode :: String -> [Data.Word.Word8]
encode _ = []
