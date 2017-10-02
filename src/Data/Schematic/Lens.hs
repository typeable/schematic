{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Schematic.Lens where

import Data.Proxy
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
  type ByRevision r (s ': rs) ('S i) = ByRevision r rs i

  flens p f (x :& xs) = fmap (x :&) (flens p f xs)
  {-# INLINE flens #-}

  fget k = getConst . flens k Const
  {-# INLINE fget #-}

  fput y = getIdentity . flens Proxy (\_ -> Identity y)
  {-# INLINE fput #-}

-- This is an internal convenience stolen from the @lens@ library.
lens
  :: Functor f
  => (t -> s)
  -> (t -> a -> b)
  -> (s -> f a)
  -> t
  -> f b
lens sa sbt afb s = fmap (sbt s) $ afb (sa s)
{-# INLINE lens #-}

-- | A partial relation that gives the indices of a sublist in a larger list.
type family FImage (rs :: [(Symbol, Schema)]) (ss :: [(Symbol, Schema)]) :: [Nat] where
  FImage '[] ss = '[]
  FImage ('(fn,s) ': rs) ss = FIndex fn ss ': FImage rs ss

class is ~ FImage rs ss
  => FSubset (rs :: [(Symbol, Schema)]) (ss :: [(Symbol, Schema)]) is where
  -- | This is a lens into a slice of the larger record. Morally, we have:
  --
  -- > fsubset :: Lens' (Rec FieldRepr ss) (Rec FieldRepr rs)
  fsubset
    :: Functor g
    => (Rec FieldRepr rs -> g (Rec FieldRepr rs))
    -> Rec FieldRepr ss
    -> g (Rec FieldRepr ss)

  -- | The getter of the 'fsubset' lens is 'fcast', which takes a larger record
  -- to a smaller one by forgetting fields.
  fcast
    :: Rec FieldRepr ss
    -> Rec FieldRepr rs
  fcast = getConst . fsubset Const
  {-# INLINE fcast #-}

  -- | The setter of the 'fsubset' lens is 'freplace', which allows a slice of
  -- a record to be replaced with different values.
  freplace
    :: Rec FieldRepr rs
    -> Rec FieldRepr ss
    -> Rec FieldRepr ss
  freplace rs = getIdentity . fsubset (\_ -> Identity rs)
  {-# INLINE freplace #-}

instance FSubset '[] ss '[] where
  fsubset = lens (const RNil) const

instance
  ( ByRevision fn ss i ~ s
  , FElem fn ss i
  , FSubset rs ss is
  ) => FSubset ( '(fn,s) ': rs) ss (i ': is) where
  fsubset = lens (\ss -> fget Proxy ss :& fcast ss) set
    where
      set :: Rec FieldRepr ss -> Rec FieldRepr ( '(fn,s) ': rs) -> Rec FieldRepr ss
      set ss (r :& rs) = fput r $ freplace rs ss
