{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_orville_postgresql (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "orville_postgresql"
version :: Version
version = Version [1,1,0,0] []

synopsis :: String
synopsis = "A Haskell library for PostgreSQL"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
