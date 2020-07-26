{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE Unsafe            #-}

module Moment.Prelude (
  module Exports
) where

import Prelude as Exports (
      read
    , length
    , take
    , drop
    , return
    , (.)
    , ($)
    , null
    , takeWhile
    , scanl
    , replicate
    , concat
    , lookup
    , elem
    , replicate
    , foldr
    , Functor(..)
    , Integer
    , Num(..)
    )

import Data.Tuple   as Exports
import Data.Maybe   as Exports
import Data.String  as Exports

import GHC.IO       as Exports -- ^ Unsafe. The module itself isn't safe.
import GHC.Prim     as Exports -- ^ Unsafe. The module itself isn't safe.
import GHC.Types    as Exports -- ^ Unsafe. The module itself isn't safe.
import GHC.Exts     as Exports
--import GHC.Num      as Exports
import Numeric      as Exports
import GHC.Real     as Exports
import GHC.Show     as Exports
import GHC.Read     as Exports
import GHC.Magic    as Exports

-- Base types
import Data.Int     as Exports
import Data.Bool    as Exports hiding (bool)
import Data.Char    as Exports (Char)

-- Base typeclasses
import Data.Eq        as Exports
import Data.Ord       as Exports
#if MIN_VERSION_base(4,9,0)
import Data.Monoid    as Exports (Monoid(..), mempty)
import Data.Semigroup as Exports (Semigroup(..), (<>)) -- ^ Since: 4.9.0.0
#else
import Data.Monoid    as Exports (Monoid(..), mempty, (<>))
#endif
