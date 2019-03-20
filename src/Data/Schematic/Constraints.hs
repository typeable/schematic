{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE EmptyCase           #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module Data.Schematic.Constraints where

import Data.Singletons.Prelude.List
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Text as T
import GHC.Generics (Generic)
import GHC.Natural


singletons [d|
  data TextConstraint' s n
    = TEq n
    | TLt n
    | TLe n
    | TGt n
    | TGe n
    | TRegex s
    | TEnum [s]
    deriving (Eq, Show, Ord, Generic)

  data NumberConstraint' n
    = NLe n
    | NLt n
    | NGt n
    | NGe n
    | NEq n
    deriving (Eq, Show, Ord, Generic)

  data ArrayConstraint' n = AEq n deriving (Eq, Show, Ord, Generic)
  |]

type TextConstraintT = TextConstraint' Text Natural
type TextConstraint = TextConstraint' Symbol Nat
type NumberConstraintT = NumberConstraint' Natural
type NumberConstraint = NumberConstraint' Nat
type ArrayConstraintT = ArrayConstraint' Natural
type ArrayConstraint = ArrayConstraint' Nat
