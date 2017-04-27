{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

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
    data Type
      = Text
      | Number
      | Array
      | Object
      | Null

  |]

type family CRepr (ty :: k) :: Type

type family Repr (ty :: k1) :: k2 where
  Repr 'Text = String -- because of matchRegex
  Repr 'Number = Double

class Constrained c ty | c -> ty where
  predicate :: Repr ty -> Bool

data LengthEqNat nat

instance KnownNat nat => Constrained (LengthEqNat nat) Text where
  predicate n = natVal (Proxy @nat) == fromIntegral (length n)

type instance CRepr (LengthEqNat nat) = Number

type family Verifiable (els :: k) (t :: z) :: Constraint where
  Verifiable '[]        ty = ()
  Verifiable (e : els) ty = (Constrained e (CRepr e), Verifiable els ty)

data Exists f where
  Exists :: f a -> Exists f

data C els =
  forall els ty. Verifiable els ty => C (Rec Proxy els)

data Schema where
  SchemaText :: Schema
  SchemaNumber :: Schema
  SchemaObjCons :: KnownSymbol label
    => Proxy label
    -> Schema -- new elem
    -> Schema -- tail
    -> Schema
  SchemaObjNil :: Schema

class Schematic ty where
  rep :: Proxy ty -> Schema

-- | Signals it's appropriate as a top-level json type
-- class Schematic ty => TopLevel ty

data c :% s

instance Schematic s => Schematic (c :% s) where
  rep _ = rep $ Proxy @s

data NilObj

instance Schematic NilObj where
  rep _ = SchemaObjNil

data (:**) (label :: Symbol) e (s :: Type)

instance (KnownSymbol label, Schematic e, Schematic s)
  => Schematic (label :** e s) where
  rep _ = SchemaObjCons
    (Proxy @label)
    (rep $ Proxy @e)
    (rep $ Proxy @s)

data JText

instance Schematic JText where
  rep _ = SchemaText

-- type TestStruct
--   = "carrier" (JString :% LengthEqNat 2 :% Regexp "[a-zA-Z]+")
--   :& "booking_classes" (JArray :% (LengthArrLe 5) (JString :% Regexp "[a-zA-Z]{2}"))
