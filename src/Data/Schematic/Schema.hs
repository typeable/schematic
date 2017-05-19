{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module Data.Schematic.Schema where

import Control.Applicative
import Control.Category ((<<<), (>>>))
import Control.Monad
import Data.Aeson as J
import Data.Aeson.Types as J
import Data.Eq.Deriving (deriveEq1)
import Data.Foldable as F
import Data.Functor.Classes
import Data.HashMap.Strict as H
import Data.Kind
import Data.Maybe
import Data.Scientific
import Data.Singletons.Decide
import Data.Singletons.Prelude.List
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Text as T
import Data.Vector as V
import Data.Vinyl hiding (Dict)
import Data.Vinyl.Functor
import GHC.Generics (Generic)
import Test.SmallCheck.Series
import Text.Show.Deriving (deriveShow1)


type family CRepr (s :: Schema) :: Type where
  CRepr (SchemaText cs)  = TextConstraint
  CRepr (SchemaNumber cs) = NumberConstraint
  CRepr (SchemaObject fs) = (String, Schema)
  CRepr (SchemaArray ar s) = ArrayConstraint

data TextConstraint
  = TEq Nat
  | TLe Nat
  | TGt Nat
  | Regex Symbol
  deriving (Generic)

data instance Sing (tc :: TextConstraint) where
  STextLengthEq :: Sing n -> Sing (TEq n)
  STextLengthLe :: Sing n -> Sing (TLe n)
  STextLengthGt :: Sing n -> Sing (TGt n)

instance Eq (Sing (TEq n)) where a == b = True
instance Eq (Sing (TLe n)) where a == b = True
instance Eq (Sing (TGt n)) where a == b = True

data NumberConstraint
  = NLe Nat
  | NGt Nat
  | NEq Nat
  deriving (Generic)

data instance Sing (nc :: NumberConstraint) where
  SNumberEq :: Sing n -> Sing (NEq n)
  SNumberGt :: Sing n -> Sing (NGt n)
  SNumberLe :: Sing n -> Sing (NLe n)

instance Eq (Sing (NEq n)) where a == b = True
instance Eq (Sing (NLe n)) where a == b = True
instance Eq (Sing (NGt n)) where a == b = True

data ArrayConstraint
  = AEq Nat
  deriving (Generic)

data instance Sing (ac :: ArrayConstraint) where
  SArrayEq :: Sing n -> Sing (AEq n)

instance Eq (Sing (AEq n)) where a == b = True

data Schema
  = SchemaText [TextConstraint]
  | SchemaNumber [NumberConstraint]
  | SchemaObject [(Symbol, Schema)]
  | SchemaArray [ArrayConstraint] Schema
  | SchemaNull
  deriving (Generic)

data instance Sing (schema :: Schema) where
  SSchemaText :: Sing tcs -> Sing (SchemaText tcs)
  SSchemaNumber :: Sing ncs -> Sing (SchemaNumber ncs)
  SSchemaArray :: Sing acs -> Sing schema -> Sing (SchemaArray acs schema)
  SSchemaObject :: Sing fields -> Sing (SchemaObject fields)
  SSchemaNull :: Sing SchemaNull

instance Eq (Sing (SchemaText cs)) where a == b = True
instance Eq (Sing (SchemaNumber cs)) where a == b = True
instance Eq (Sing SchemaNull) where a == b = True
instance Eq (Sing (SchemaArray as s)) where a == b = True
instance Eq (Sing (SchemaObject cs)) where a == b = True

data FieldRepr :: (Symbol, Schema) -> Type where
  FieldRepr :: KnownSymbol name => JsonRepr schema -> FieldRepr '(name, schema)

instance Eq (JsonRepr schema) => Eq (FieldRepr '(name, schema)) where
  FieldRepr a == FieldRepr b = a == b

instance
  ( KnownSymbol name
  , Serial m (JsonRepr schema) )
  => Serial m (FieldRepr '(name, schema)) where
  series = FieldRepr <$> series

data JsonRepr :: Schema -> Type where
  ReprText :: Text -> JsonRepr (SchemaText cs)
  ReprNumber :: Scientific -> JsonRepr (SchemaNumber cs)
  ReprNull :: JsonRepr SchemaNull
  ReprArray :: V.Vector (JsonRepr s) -> JsonRepr (SchemaArray cs s)
  ReprObject :: Rec FieldRepr fs -> JsonRepr (SchemaObject fs)

instance Eq (Rec FieldRepr fs) => Eq (JsonRepr (SchemaObject fs)) where
  ReprObject a == ReprObject b = a == b

instance Eq (JsonRepr (SchemaText cs)) where
  ReprText a == ReprText b = a == b

instance Eq (JsonRepr (SchemaNumber cs)) where
  ReprNumber a == ReprNumber b = a == b

instance Eq (JsonRepr SchemaNull) where
  ReprNull == ReprNull = True

instance Eq (JsonRepr s) => Eq (JsonRepr (SchemaArray as s)) where
  ReprArray a == ReprArray b = a == b

instance (SingI schema) => J.FromJSON (JsonRepr schema) where
  parseJSON value = case sing :: Sing schema of
    SSchemaText _    -> withText "String" (pure . ReprText) value
    SSchemaNumber _  -> withScientific "Number" (pure . ReprNumber) value
    SSchemaNull      -> pure ReprNull
    SSchemaArray c s -> withArray "Array" f value
      where
        f = fmap ReprArray . (withSingI s $ traverse parseJSON)
    SSchemaObject fs -> ReprObject <$> withObject "Object" (demoteFields fs) value

demoteFields
  :: SList s
  -> H.HashMap Text J.Value
  -> Parser (Rec FieldRepr s)
demoteFields SNil h
  | H.null h  = pure RNil
  | otherwise = mzero
demoteFields (SCons (STuple2 (n :: Sing fn) s) tl) h = withKnownSymbol n $ do
  let fieldName = T.pack $ symbolVal (Proxy @fn)
  fieldRepr <- case H.lookup fieldName h of
    Just v  -> FieldRepr <$> (withSingI s $ parseJSON v)
    Nothing -> mzero
  (fieldRepr :&) <$> demoteFields tl h

instance J.ToJSON (JsonRepr a) where
  toJSON ReprNull       = J.Null
  toJSON (ReprText t)   = J.String t
  toJSON (ReprNumber n) = J.Number n
  toJSON (ReprArray v)  = J.Array $ toJSON <$> v
  toJSON (ReprObject r) = J.Object . H.fromList . fold $ r
    where
      extract :: forall name s . (KnownSymbol name)
        => FieldRepr '(name, s)
        -> (Text, Value)
      extract (FieldRepr s) = (T.pack $ symbolVal $ Proxy @name, toJSON s)
      fold :: Rec FieldRepr fs -> [(Text, J.Value)]
      fold = \case
        RNil                   -> []
        fr@(FieldRepr s) :& tl -> (extract fr) : fold tl

class FalseConstraint

type family TopLevel (spec :: Schema) :: Constraint where
  TopLevel (SchemaArray a e) = ()
  TopLevel (SchemaObject o)  = ()
  TopLevel spec              = FalseConstraint
