{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module Data.Schematic.Schema where

import Data.Functor.Foldable
import Data.Kind hiding (Type)
import Data.Proxy
import Data.Schematic.Validation
import Data.Text (Text)
import GHC.Generics
import GHC.TypeLits (KnownNat, natVal, KnownSymbol, symbolVal, Symbol, Nat)


class Constrained (ty :: JType) c where
  cRep :: Proxy ty -> Proxy c -> CRepr ty

-- Validators

data LengthEqNat nat

instance KnownNat nat => Constrained 'JText (LengthEqNat nat) where
  cRep _ _ = LengthEq $ natVal (Proxy @nat)

data Gt nat

instance KnownNat nat => Constrained 'JNumber (Gt nat) where
  cRep _ _ = Gt $ natVal (Proxy @nat)

instance KnownNat nat => Constrained 'JArray (LengthEqNat nat) where
  cRep _ _ = LengthArrEq $ natVal (Proxy @nat)

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
  ( Constrained 'JArray c
  , Verifiable 'JArray cs )
  => Verifiable 'JArray (c ': cs) where
  schema _ _ = cRep (Proxy @'JArray) (Proxy @c)
    : schema (Proxy @'JArray) (Proxy @cs)

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

class SchemaConstructor jty where
  constructor :: Proxy jty -> [CRepr jty] -> Schema

instance SchemaConstructor 'JText where
  constructor _ = Fix . SchemaText SJText

instance SchemaConstructor 'JNumber where
  constructor _ = Fix . SchemaNumber SJNumber

instance SchemaConstructor 'JObject where
  constructor _ = Fix . SchemaObject SJObject

class Schematic ty where
  type JT (ty :: k) :: JType
  build :: Proxy ty -> Schema

data SText cs

instance (Verifiable 'JText cs) => Schematic (SText cs) where
  type JT (SText cs) = 'JText
  build _ = constructor (Proxy @(JT (SText cs))) $ schema (Proxy @JText) (Proxy @cs)

data SNumber cs

instance (Verifiable 'JNumber cs) => Schematic (SNumber cs) where
  type JT (SNumber cs) = 'JNumber
  build _ = constructor (Proxy @(JT (SNumber cs)))
    $ schema (Proxy @'JNumber) (Proxy @cs)

instance
  ( Verifiable (JT ty) cs
  , SchemaConstructor (JT ty) )
  => Schematic (Field field ty cs) where
  type JT (Field field ty cs) = JT ty
  build _ = constructor (Proxy @(JT (Field field ty cs)))
    $ schema (Proxy @(JT (Field field ty cs))) (Proxy @cs)

data SObject els

instance (Verifiable 'JObject els) => Schematic (SObject els) where
  type JT (SObject els) = 'JObject
  build _ =
    constructor (Proxy @(JT (SObject els)))
      $ schema (Proxy @(JT (SObject els))) (Proxy @els)

data SArray cs s

instance
  ( Schematic s
  , Verifiable 'JArray cs )
  => Schematic (SArray cs s) where
  type JT (SArray cs s) = 'JArray
  build _ =
    Fix $ SchemaArray
      SJArray
      (schema (Proxy @'JArray) (Proxy @cs))
      (build (Proxy @s))

data Generate name ty

instance
  ( Verifiable JText cs
  , KnownSymbol name, Schematic ty )
  => Schematic (Generate name (SText cs)) where
  type JT (Generate name (SText cs)) = JT (SText cs)
  build _ = build (Proxy @(SText cs))

instance
  ( Verifiable JNumber cs
  , KnownSymbol name, Schematic ty )
  => Schematic (Generate name (SNumber cs)) where
  type JT (Generate name (SNumber cs)) = JT (SNumber cs)
  build _ = build (Proxy @(SNumber cs))

instance
  ( Verifiable JArray cs
  , Schematic s )
  => Schematic (Generate name (SArray cs s)) where
  type JT (Generate name (SArray cs s)) = JT (SArray cs s)
  build _ = build (Proxy @(SArray cs s))

instance
  (Verifiable JObject els)
  => Schematic (Generate name (SObject els)) where
  type JT (Generate name (SObject els)) = JT (SObject els)
  build _ = build (Proxy @(SObject els))

-- | Shows it's top-level definition according to the json-schema
class TopLevel spec

instance Schematic (SObject els) => TopLevel (SObject els)

instance Schematic (SArray cs s) => TopLevel (SArray cs s)

-- | TODO: abstract over objects and arrays
-- toSchema
--   :: forall els (e :: [k]). (e ~ '[], Verifiable (JT (SObject els)) e, Schematic (SObject els))
--   => Proxy (SObject els)
--   -> Schema
-- toSchema p = constructor p $ schema (Proxy @(JT (SObject els))) (Proxy @els)
