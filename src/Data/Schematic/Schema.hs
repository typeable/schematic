{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module Data.Schematic.Schema where

import Data.Functor.Foldable
import Data.Singletons.TH
import Data.Kind hiding (Type)
import Data.Proxy
import Data.Type.Equality
import Data.Vinyl
import Data.Vinyl.Functor
import GHC.TypeLits (KnownNat, natVal, KnownSymbol, symbolVal, Symbol, Nat)

singletons [d|
    data Type els
      = Text
      | Number
      | Array
      | Object els
      | Null

  |]

type family CRepr (ty :: k) :: Type els

type family Repr (ty :: k1) :: k2 where
  Repr 'Text = String -- because of matchRegex
  Repr 'Number = Double

class Constrained c ty where
  predicate :: Repr ty -> Bool

data LengthEqNat nat

instance KnownNat nat => Constrained (LengthEqNat nat) Text where
  predicate n = natVal (Proxy @nat) == fromIntegral (length n)

type instance CRepr (LengthEqNat nat) = Number

type family Verifiable (els :: k) (t :: z) :: Constraint where
  Verifiable '[]       ty = ()
  Verifiable (e : els) ty = (Constrained e (CRepr e), Verifiable els ty)

data Schema (ty :: Type k) where
  SchemaText :: Verifiable cs Text => Rec Sing cs -> Schema Text
  SchemaNumber :: Verifiable cs Number => Rec Sing cs -> Schema Number
  SchemaObjCons :: (KnownSymbol label, Verifiable cs t)
    => Proxy label
    -> Rec Sing cs
    -> Schema t -- new elem
    -> Schema (Object tys) -- tail
    -> Schema (Object (t ': tys))
  SchemaObjNil :: Schema (Object '[])

class Schematic ty where
  type SchemaOf ty :: Type t
  rep :: Proxy ty -> Schema (SchemaOf ty)

data c :% s

instance Schematic s => Schematic (c :% s) where
  type SchemaOf (c :% s) = SchemaOf s
  rep _ = rep $ Proxy @s

data NilObj

instance Schematic NilObj where
  type SchemaOf NilObj = Object '[]
  rep _ = SchemaObjNil

data (:**) (label :: Symbol) e (s :: k)

instance (KnownSymbol label, Schematic e, Schematic s)
  => Schematic (label :** e s) where
  type SchemaOf (label :** e s) = SchemaOf s
  rep _ = SchemaObjCons
    (Proxy @label)
    RNil
    (rep $ Proxy @e)
    (rep $ Proxy @s)

data JText

instance Schematic JText where
  rep _ = SchemaText

-- | Signals it's appropriate as a top-level json type
-- class Schematic ty => TopLevel ty

-- -- type TestStruct
-- --   = "carrier" (JString :% LengthEqNat 2 :% Regexp "[a-zA-Z]+")
-- --   :& "booking_classes" (JArray :% (LengthArrLe 5) (JString :% Regexp "[a-zA-Z]{2}"))
