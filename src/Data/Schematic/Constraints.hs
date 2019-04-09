{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module Data.Schematic.Constraints where

import Data.Schematic.Compat
import Data.Singletons.Prelude
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Text as T
import GHC.Generics (Generic)


singletons [d|
  data TextConstraint' s n
    = TEq n
    | TLt n
    | TLe n
    | TGt n
    | TGe n
    | TRegex s
    | TEnum [s]
    deriving (Eq, Show, Generic)

  data NumberConstraint' n
    = NLe n
    | NLt n
    | NGt n
    | NGe n
    | NEq n
    deriving (Eq, Show, Generic)

  data ArrayConstraint' n = AEq n deriving (Eq, Show, Generic)
  |]

type TextConstraintT = TextConstraint' Text DeNat
type TextConstraint = TextConstraint' Symbol Nat
type NumberConstraintT = NumberConstraint' DeNat
type NumberConstraint = NumberConstraint' Nat
type ArrayConstraintT = ArrayConstraint' DeNat
type ArrayConstraint = ArrayConstraint' Nat
