module Aural.Either
  ( unsafeFromRight
  ) where

unsafeFromRight :: Either String a -> a
unsafeFromRight e =
  case e of
    Left l -> error l
    Right r -> r
