module Aural.Version
  ( version
  ) where

import qualified Data.Version
import qualified Paths_aural

-- | Aural's version number. This is not necessarily associated with the Rocket
-- League version number. For example, Aural version 1.2.3.4 may be able to
-- parse replays from Rocket League version 1.63.
--
-- Aural follows the [Package Versioning Policy](https://pvp.haskell.org) with
-- the additional constraint of the first number always being 1. So you can
-- think of it as [Semantic Versioning](https://semver.org) with an extra
-- number at the beginning.
--
-- Please see [Aural's releases](https://github.com/tfausak/aural/releases) on
-- GitHub for a detailed list of changes.
version :: Data.Version.Version
version = Paths_aural.version
