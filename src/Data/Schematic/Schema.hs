{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module Data.Schematic.Schema where

import Data.Kind hiding (Type)
import Data.Proxy
import Data.Text (Text)
import GHC.TypeLits (KnownNat, natVal, KnownSymbol, symbolVal, Symbol, Nat)


-- | Type Tags
data JType
  = JText
  | JNumber
  | JArray
  | JObject
  | JNull

type family Repr (ty :: JType) :: * where
  Repr 'JText = String -- because of matchRegex
  Repr 'JNumber = Double

class Constrained (ty :: JType) c where
  cRep :: Proxy ty -> Proxy c -> CRepr ty

-- Validators

data LengthEqNat nat

instance KnownNat nat => Constrained 'JText (LengthEqNat nat) where
  cRep _ _ = LengthEq $ natVal (Proxy @nat)

data Gt nat

instance KnownNat nat => Constrained 'JNumber (Gt nat) where
  cRep _ _ = Gt $ natVal (Proxy @nat)

-- /Validators

data Field field ty schema

data Empty

class Verifiable ty cs where
  schema :: Proxy ty -> Proxy cs -> [CRepr ty]

instance
  ( Constrained 'JText c
  , Verifiable 'JText cs )
  => Verifiable 'JText (c ': cs) where
  schema _ _ = cRep (Proxy @'JText) (Proxy @c)
    : schema (Proxy @'JText) (Proxy @cs)

instance
  ( Constrained 'JNumber c
  , Verifiable 'JNumber cs )
  => Verifiable 'JNumber (c ': cs) where
  schema _ _ = cRep (Proxy @'JNumber) (Proxy @c)
    : schema (Proxy @'JNumber) (Proxy @cs)

instance Verifiable jty '[] where
  schema _ _ = []

instance
  ( SchemaConstructor jty
  , Verifiable 'JObject fs
  , KnownSymbol field
  , Verifiable jty cs )
  => Verifiable 'JObject ((Field field jty cs) ': fs) where
  schema _ _ = (fieldName, fieldSchema) : schema (Proxy @'JObject) (Proxy @fs)
    where
      fieldName   = symbolVal (Proxy @field)
      fieldSchema = constructor (Proxy @jty) $ schema (Proxy @jty) (Proxy @cs)

type family CRepr (jty :: JType) :: * where
  CRepr JText  = TextConstraint
  CRepr JNumber = NumberConstraint
  CRepr JObject = (String, Schema)

data TextConstraint
  = LengthEq Integer
  | LengthLe Integer
  | LengthGt Integer
  deriving (Show, Eq)

data NumberConstraint
  = Le Integer
  | Gt Integer
  | Eq Integer
  deriving (Show, Eq)

data Schema
  = SchemaText [TextConstraint]
  | SchemaNumber [NumberConstraint]
  | SchemaObject [(String, Schema)]
  | SchemaArray Schema
  deriving (Show, Eq)

-- | Apply constraint for each element of list
-- type family AllSatisfy (c :: k -> Constraint) (s :: [k]) :: Constraint where
--   AllSatisfy c '[]       = ()
--   AllSatisfy c (a ': as) = (c a, AllSatisfy c as)

class SchemaConstructor jty where
  constructor :: Proxy jty -> [CRepr jty] -> Schema

instance SchemaConstructor 'JText where
  constructor _ = SchemaText

instance SchemaConstructor 'JNumber where
  constructor _ = SchemaNumber

instance SchemaConstructor 'JObject where
  constructor _ = SchemaObject

class (SchemaConstructor (JT ty)) => Schematic ty where
  type JT (ty :: k) :: JType
  build :: Proxy ty -> Schema

data SText cs

instance (Verifiable 'JText cs) => Schematic (SText cs) where
  type JT (SText cs) = 'JText
  build _ = constructor (Proxy @(JT (SText cs))) $ schema (Proxy @JText) (Proxy @cs)

data SNumber cs

instance (Verifiable 'JNumber cs) => Schematic (SNumber cs) where
  type JT (SNumber cs) = 'JNumber
  build _ = constructor (Proxy @(JT (SNumber cs))) $ schema (Proxy @'JNumber) (Proxy @cs)

instance (Verifiable (JT ty) cs, SchemaConstructor (JT ty)) => Schematic (Field field ty cs) where
  type JT (Field field ty cs) = JT ty
  build _ = constructor (Proxy @(JT (Field field ty cs)))
    $ schema (Proxy @(JT (Field field ty cs))) (Proxy @cs)

data SObject els

instance (Verifiable 'JObject els) => Schematic (SObject els) where
  type JT (SObject els) = 'JObject
  build _ =
    constructor (Proxy @(JT (SObject els)))
      $ schema (Proxy @(JT (SObject els))) (Proxy @els)

-- | Shows it's top-level definition according to the json-schema
class TopLevel spec

instance Schematic (SObject els) => TopLevel (SObject els)

-- | TODO: abstract over objects and arrays
-- toSchema
--   :: forall els (e :: [k]). (e ~ '[], Verifiable (JT (SObject els)) e, Schematic (SObject els))
--   => Proxy (SObject els)
--   -> Schema
-- toSchema p = constructor p $ schema (Proxy @(JT (SObject els))) (Proxy @els)
