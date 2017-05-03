{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module Data.Schematic.Schema where

import Data.Functor.Foldable
import Data.Kind hiding (Type)
import Data.Proxy
import Data.Singletons.TH
import Data.Text (Text)
import Data.Type.Equality
import Data.Vinyl
import Data.Vinyl.Functor
import GHC.Exts
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

data LengthEqNat nat

instance KnownNat nat => Constrained 'JText (LengthEqNat nat) where
  cRep _ _ = LengthEq $ natVal (Proxy @nat)

data Field field jty schema

instance (Schematic jty, Verifiable (JT jty) cs, KnownSymbol field)
  => Constrained JObject (Field field jty cs) where
  cRep _ _ =
    (symbolVal (Proxy @field), constructor (Proxy @jty) $ schema (Proxy @(JT jty)) (Proxy @cs))

class Verifiable ty cs where
  schema :: Proxy ty -> Proxy cs -> [CRepr ty]

instance Verifiable JText '[] where
  schema _ _ = []

instance
  (Constrained JText c, Verifiable JText cs) => Verifiable JText (c ': cs) where
  schema _ _ = schema (Proxy @JText) (Proxy @cs) ++ schema (Proxy @JText) (Proxy @cs)

instance Verifiable JNumber '[] where
  schema _ _ = []

instance
  (Constrained JNumber c, Verifiable JNumber cs) => Verifiable JNumber (c ': cs) where
  schema _ _ = schema (Proxy @JNumber) (Proxy @cs) ++ schema (Proxy @JNumber) (Proxy @cs)

instance Verifiable JObject '[] where
  schema _ _ = []

instance
  (Constrained JObject c, Verifiable JObject cs, KnownSymbol field)
  => Verifiable JObject ((Field field jty c) ': cs) where
  schema _ _ = cRep (Proxy @JObject) (Proxy @c) : schema (Proxy @JObject) (Proxy @cs)

type family VerifiableFields (ty :: JType) (els :: [k]) :: Constraint where
  VerifiableFields ty '[] = ()
  VerifiableFields ty ((Field f jty s) ': elss)
    = (Constrained jty s, VerifiableFields ty elss)

type family CRepr (jty :: JType) :: * where
  CRepr JText  = TextConstraint
  CRepr JNumber = NumberConstraint
  CRepr JObject = (String, Schema)

data TextConstraint
  = LengthEq Integer
  | LengthLe Integer
  | LengthGt Integer

data NumberConstraint
  = Le Integer
  | Gt Integer
  | Eq Integer

data Schema
  = SchemaText [TextConstraint]
  | SchemaNumber [NumberConstraint]
  | SchemaObject [(String, Schema)]
  | SchemaArray Schema

-- | Apply constraint for each element of list
type family AllSatisfy (c :: k -> Constraint) (s :: [k]) :: Constraint where
  AllSatisfy c '[]       = ()
  AllSatisfy c (a ': as) = (c a, AllSatisfy c as)

class Schematic ty where
  type JT ty :: JType
  constructor :: Proxy ty -> [CRepr (JT ty)] -> Schema

data SText cs

instance (Verifiable JText cs) => Schematic (SText cs) where
  type JT (SText cs) = JText
  constructor _ = SchemaText

-- example
type TextSchema = SText '[LengthEqNat 3]

data SNumber cs

instance (Verifiable JNumber cs) => Schematic (SNumber cs) where
  type JT (SNumber cs) = JNumber
  constructor _ = SchemaNumber

data SObject els

instance (els ~ '[k], AllSatisfy Schematic els) => Schematic (SObject els) where
  type JT (SObject els) = JObject
  constructor _ = SchemaObject

-- | Shows it's top-level definition according to the json-schema
class TopLevel spec

instance Schematic (SObject spec) => TopLevel (SObject els)

-- toSchema :: forall spec cs. (Schematic spec, VerifiableFields (JT spec), TopLevel spec) => Proxy spec -> Schema
-- toSchema pspec = constructor pspec $ schema (Proxy @(JT spec)) pspec

type TestStruct = SObject '[ '("carrier", [SText '[LengthEqNat 3]]) ]
