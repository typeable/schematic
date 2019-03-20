{-# LANGUAGE CPP #-}
module Data.Schematic.Compat where

import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TypeLits

type DeNat = Demote Nat
-- ^ Demote Nat is depends on version of singletons

#if !MIN_VERSION_base(4,11,0)
type (:+++) a b = (:++) a b
#else
type (:+++) a b = (++) a b
#endif
