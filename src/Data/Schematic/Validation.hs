{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Schematic.Validation where

import Control.Applicative
import Control.Category ((<<<), (>>>))
import Data.Eq.Deriving (deriveEq1)
import Data.Foldable as F
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Kind
import Data.Maybe
import Data.Singletons.Decide
import Data.Singletons.TH
import Data.Text as T
import Data.Vinyl hiding (Dict)
import Data.Vinyl.Functor
import Text.Show.Deriving (deriveShow1)


-- | Type Tags
data JType
  = JText
  | JNumber
  | JArray
  | JObject
  | JNull
  deriving (Show, Eq)

genSingletons [ ''JType ]

instance Show (Sing JText) where showsPrec 4 _ = showString "SJText"
instance Show (Sing JNumber) where showsPrec 4 _ = showString "SJNumber"
instance Show (Sing JArray) where showsPrec 4 _ = showString "SJArray"
instance Show (Sing JObject) where showsPrec 4 _ = showString "SJObject"
instance Show (Sing JNull) where showsPrec 4 _ = showString "SJNull"

instance Eq (Sing JText) where (==) _ _ = True
instance Eq (Sing JNumber) where (==) _ _ = True
instance Eq (Sing JArray) where (==) _ _ = True
instance Eq (Sing JObject) where (==) _ _ = True
instance Eq (Sing JNull) where (==) _ _ = True

type family CRepr (jty :: JType) :: Type where
  CRepr JText  = TextConstraint
  CRepr JNumber = NumberConstraint
  CRepr JObject = (String, Schema)
  CRepr JArray = ArrayConstraint

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

data ArrayConstraint
  = LengthArrEq Integer
  deriving (Show, Eq)

type ObjectConstraint s = [(String, s)]

data SchemaF s
  = SchemaText [TextConstraint]
  | SchemaNumber [NumberConstraint]
  | SchemaObject (ObjectConstraint s)
  | SchemaArray [ArrayConstraint] s
  | SchemaNull
  deriving (Functor)

type Schema = Fix SchemaF

deriveEq1 ''SchemaF

deriveShow1 ''SchemaF
