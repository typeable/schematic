{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Schematic.Schema where

import Data.Maybe
import Data.Proxy
import GHC.TypeLits
import Text.Regex


-- JSON types
data JText
data JNumber
data JArray
data JObject
data JNull

type family Repr (ty :: k1) :: k2 where
  Repr JText = String -- because of matchRegex
  Repr JNumber = Double

class Schema schema where
  type Term subschema
  predicate :: Repr (Term ty) -> Bool

data Regexp (s :: Symbol) = Regexp

instance KnownSymbol reg => Schema (Regexp reg) subschema where
  type Term (Regexp reg) subschema = Term ((), subschema)
  predicate = isJust . matchRegex (mkRegex $ symbolVal (Proxy :: Proxy reg))

data LengthEqNat (n :: Nat)
data LengthGtNat (n :: Nat)
data LengthGeNat (n :: Nat)
data LengthLtNat (n :: Nat)
data LengthLeNat (n :: Nat)

instance KnownNat n => Schema (LengthEqNat n) subschema where
  type Term (LengthEqNat n) s = Term s
  predicate s = natVal (Proxy :: Proxy n) == fromIntegral (length s)
    && predicate

instance KnownNat n => Schema (LengthGtNat n) rest where
  type Term (LengthGtNat n) = JNumber
  predicate s = natVal (Proxy :: Proxy n) > fromIntegral (length s)

instance KnownNat n => Schema (LengthGeNat n) rest where
  type Term (LengthGeNat n) = JNumber
  predicate s = natVal (Proxy :: Proxy n) >= fromIntegral (length s)

instance KnownNat n => Schema (LengthLtNat n) rest where
  type Term (LengthLtNat n) = JNumber
  predicate s = natVal (Proxy :: Proxy n) < fromIntegral (length s)

instance KnownNat n => Schema (LengthLeNat n) rest where
  type Term (LengthLeNat n) = JNumber
  predicate s = natVal (Proxy :: Proxy n) <= fromIntegral (length s)

-- | Corresponds to [] or {} json values.
data EmptyObj = EmptyObj

data ConsObj (s :: Symbol) jsval = ConsObj

-- data Schema a where
--   (:*:) :: KnownSymbol label => label -> Schema l -> Schema r -> Schema a
--   (:%:) :: Schema c a => Schema a -> c a -> Schema a
--   Nil   :: Schema a

data (s :: Symbol) :+: obj = (:+:) a b

infixl 6 :+:
infixl 6 :*:
infixl 9 :%:

schema :: Schema ()
schema = "foo" :%: (Regexp "foo") :*: Nil
