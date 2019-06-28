module Aural.Main
  ( defaultMain
  ) where

import qualified Aural.Json
import qualified Aural.Utf8
import qualified Data.ByteString.Lazy

defaultMain :: IO ()
defaultMain = Data.ByteString.Lazy.interact
  ( Data.ByteString.Lazy.pack
  . Aural.Utf8.encode
  . Aural.Json.encode
  . map (either (error . mappend "invalid JSON: " . show) id)
  . Aural.Json.decode
  . map (either (error . mappend "invalid UTF-8: " . show) id)
  . Aural.Utf8.decode
  . Data.ByteString.Lazy.unpack
  )
