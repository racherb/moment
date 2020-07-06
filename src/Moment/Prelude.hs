{-# LANGUAGE NoImplicitPrelude #-}

module Moment.Prelude (
  module Exports
) where

import Prelude as Exports (
    read,
    show,
    length,
    take,
    drop,
    return,
    (.),
    ($),
    null,
    takeWhile,
    scanl,
    replicate,
    ceiling,
    concat,
    lookup,
    elem,
    replicate,
    foldr,
    fromIntegral,
    ceiling,
    quot,
    Monoid(..),
    Semigroup(..),
    Functor(..)
    )

import Data.Tuple as Exports
import Data.Maybe as Exports
import Data.String as Exports

import GHC.IO as Exports
import GHC.Num as Exports
import GHC.Real as Exports
import GHC.Show as Exports
import GHC.Read as Exports

-- Base types
import Data.Int     as Exports
import Data.Bool    as Exports hiding (bool)
import Data.Char    as Exports (Char)

-- Base typeclasses
import Data.Eq as Exports
import Data.Ord as Exports
import Data.Monoid as Exports (Monoid(..), mempty)
import Data.Semigroup as Exports (Semigroup(..), (<>))
import Data.Functor.Identity as Exports