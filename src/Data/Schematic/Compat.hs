{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
module Data.Schematic.Compat where

import Data.Singletons.Prelude
import GHC.TypeLits
#if MIN_VERSION_base(4,12,0)
import Data.Vinyl
#else
import Data.Kind
#endif


type DeNat = Demote Nat
-- ^ Demote Nat is depends on version of singletons

demote' :: forall a. (SingI a, SingKind (KindOf a)) => Demote (KindOf a)
#if MIN_VERSION_singletons(2,4,0)
type (:+++) a b = (++) a b
demote' = demote @a
#else
type (:+++) a b = (:++) a b
demote' = fromSing (sing :: Sing a)
#endif

#if MIN_VERSION_vinyl(0,9,0)
type RMapCompat fs = RMap fs
type ReifyConstraintCompat c repr fs = ReifyConstraint c repr fs
type RecordToListCompat fs = RecordToList fs
#else
type RMapCompat fs = (() :: Constraint)
type ReifyConstraintCompat c fs repr = (() :: Constraint)
type RecordToListCompat fs = (() :: Constraint)
#endif
