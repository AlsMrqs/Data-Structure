{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_Data_Structure (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "Data_Structure"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "\"Research some data-structures in Haskell\""
copyright :: String
copyright = ""
homepage :: String
homepage = ""
