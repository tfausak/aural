module Aural.Either
  ( unsafeFromRight
  ) where

import qualified GHC.Stack

-- | Returns the 'Right' value if it's available, otherwise calls 'error' with
-- the 'Left' value.
unsafeFromRight :: GHC.Stack.HasCallStack => Either String a -> a
unsafeFromRight e =
  case e of
    Left l -> error l
    Right r -> r
