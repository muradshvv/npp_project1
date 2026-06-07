{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_tower_defense (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "tower_defense"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A pure-state grid tower defense game with a terminal UI"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
