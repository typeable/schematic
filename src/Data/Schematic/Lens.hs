{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Schematic.Lens where

import Data.Proxy
import Data.Schematic.Migration
import Data.Schematic.Schema
import Data.Vinyl
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel (Nat(..))
import GHC.TypeLits (Symbol)


-- | A partial relation that gives the index of a value in a list.
type family FIndex (r :: Symbol) (rs :: [(Symbol, Schema)]) :: Nat where
  FIndex r ( '(fn, s) ': rs) = 'Z
  FIndex r (  s       ': rs) = 'S (FIndex r rs)

class i ~ FIndex fn rs => FElem (fn :: Symbol) (rs :: [(Symbol, Schema)]) (i :: Nat) where
  type ByRevision fn rs i :: Schema
  flens
    :: Functor g
    => proxy fn
    -> (FieldRepr '(fn, (ByRevision fn rs i)) -> g (FieldRepr '(fn, (ByRevision fn rs i))))
    -> Rec FieldRepr rs
    -> g (Rec FieldRepr rs)

  -- | For Vinyl users who are not using the @lens@ package, we provide a getter.
  fget
    :: proxy fn
    -> Rec FieldRepr rs
    -> FieldRepr '(fn, (ByRevision fn rs i))

  -- | For Vinyl users who are not using the @lens@ package, we also provide a
  -- setter. In general, it will be unambiguous what field is being written to,
  -- and so we do not take a proxy argument here.
  fput
    :: FieldRepr '(fn, ByRevision fn rs i)
    -> Rec FieldRepr rs
    -> Rec FieldRepr rs

instance FElem fn ('(fn, r) ': rs) 'Z where
  type ByRevision fn ('(fn, r) ': rs) 'Z = r
  flens _ f (x :& xs) = fmap (:& xs) (f x)
  {-# INLINE flens #-}
  fget k = getConst . flens k Const
  {-# INLINE fget #-}
  fput y = getIdentity . flens Proxy (\_ -> Identity y)
  {-# INLINE fput #-}

instance (FIndex r (s ': rs) ~ 'S i, FElem r rs i) => FElem r (s ': rs) ('S i) where
  type ByRevision fn (s ': rs) ('S i) = ByRevision fn rs i
  flens p f (x :& xs) = fmap (x :&) (flens p f xs)
  {-# INLINE flens #-}
  fget k = getConst . flens k Const
  {-# INLINE fget #-}
  fput y = getIdentity . flens Proxy (\_ -> Identity y)
  {-# INLINE fput #-}

type SchemaExample
  = 'SchemaObject
    '[ '("foo", 'SchemaArray '[ 'AEq 1] ('SchemaNumber '[ 'NGt 10]))
     , '("bar", 'SchemaOptional ('SchemaText '[ 'TEnum '["foo", "bar"]]))]

type TestMigration =
  'Migration "test_revision"
    '[ 'Diff '[ 'PKey "bar" ] ('Update ('SchemaText '[]))
     , 'Diff '[ 'PKey "foo" ] ('Update ('SchemaNumber '[])) ]

type VS = 'Versioned SchemaExample '[ TestMigration ]

type Ex = FIndex "initial" (AllVersions VS)
