{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module Data.Schematic.Schema where

import Control.Applicative
import Control.Monad
import Data.Aeson as J
import Data.Aeson.Types as J
import Data.HashMap.Strict as H
import Data.Kind
import Data.Maybe
import Data.Schematic.Instances ()
import Data.Schematic.Utils
import Data.Scientific
import Data.Singletons.Prelude.List
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Text as T
import Data.Vector as V
import Data.Vinyl hiding (Dict)
import Data.Vinyl.TypeLevel hiding (Nat)
import GHC.Generics (Generic)
import Prelude as P
import Test.SmallCheck.Series


type family CRepr (s :: Schema) :: Type where
  CRepr ('SchemaText cs)  = TextConstraint
  CRepr ('SchemaNumber cs) = NumberConstraint
  CRepr ('SchemaObject fs) = (String, Schema)
  CRepr ('SchemaArray ar s) = ArrayConstraint

data TextConstraint
  = TEq Nat
  | TLe Nat
  | TGt Nat
  | TRegex Symbol
  deriving (Generic)

data instance Sing (tc :: TextConstraint) where
  STEq :: Sing n -> Sing ('TEq n)
  STLe :: Sing n -> Sing ('TLe n)
  STGt :: Sing n -> Sing ('TGt n)
  STRegex :: Sing s -> Sing ('TRegex s)

instance Known (Sing n) => Known (Sing ('TEq n)) where known = STEq known
instance Known (Sing n) => Known (Sing ('TGt n)) where known = STGt known
instance Known (Sing n) => Known (Sing ('TLe n)) where known = STLe known
instance Known (Sing s) => Known (Sing ('TRegex s)) where known = STRegex known

instance Eq (Sing ('TEq n)) where _ == _ = True
instance Eq (Sing ('TLe n)) where _ == _ = True
instance Eq (Sing ('TGt n)) where _ == _ = True

data NumberConstraint
  = NLe Nat
  | NGt Nat
  | NEq Nat
  deriving (Generic)

data instance Sing (nc :: NumberConstraint) where
  SNEq :: Sing n -> Sing ('NEq n)
  SNGt :: Sing n -> Sing ('NGt n)
  SNLe :: Sing n -> Sing ('NLe n)

instance Known (Sing n) => Known (Sing ('NEq n)) where known = SNEq known
instance Known (Sing n) => Known (Sing ('NGt n)) where known = SNGt known
instance Known (Sing n) => Known (Sing ('NLe n)) where known = SNLe known

instance Eq (Sing ('NEq n)) where _ == _ = True
instance Eq (Sing ('NLe n)) where _ == _ = True
instance Eq (Sing ('NGt n)) where _ == _ = True

data ArrayConstraint
  = AEq Nat
  deriving (Generic)

data instance Sing (ac :: ArrayConstraint) where
  SAEq :: Sing n -> Sing ('AEq n)

instance Known (Sing n) => Known (Sing ('AEq n)) where known = SAEq known

instance Eq (Sing ('AEq n)) where _ == _ = True

data Schema
  = SchemaText [TextConstraint]
  | SchemaNumber [NumberConstraint]
  | SchemaObject [(Symbol, Schema)]
  | SchemaArray [ArrayConstraint] Schema
  | SchemaNull
  | SchemaOptional Schema
  deriving (Generic)

data instance Sing (schema :: Schema) where
  SSchemaText :: Known (Sing tcs) => Sing tcs -> Sing ('SchemaText tcs)
  SSchemaNumber :: Known (Sing ncs) => Sing ncs -> Sing ('SchemaNumber ncs)
  SSchemaArray :: (Known (Sing acs), Known (Sing schema)) => Sing acs -> Sing schema -> Sing ('SchemaArray acs schema)
  SSchemaObject :: Known (Sing fields) => Sing fields -> Sing ('SchemaObject fields)
  SSchemaOptional :: Known (Sing s) => Sing s -> Sing ('SchemaOptional s)
  SSchemaNull :: Sing 'SchemaNull

instance Known (Sing sl) => Known (Sing ('SchemaText sl)) where
  known = SSchemaText known
instance Known (Sing sl) => Known (Sing ('SchemaNumber sl)) where
  known = SSchemaNumber known
instance Known (Sing 'SchemaNull) where
  known = SSchemaNull
instance (Known (Sing ac), Known (Sing s)) => Known (Sing ('SchemaArray ac s)) where
  known = SSchemaArray known known
instance Known (Sing stl) => Known (Sing ('SchemaObject stl)) where
  known = SSchemaObject known
instance Known (Sing s) => Known (Sing ('SchemaOptional s)) where
  known = SSchemaOptional known

instance Eq (Sing ('SchemaText cs)) where _ == _ = True
instance Eq (Sing ('SchemaNumber cs)) where _ == _ = True
instance Eq (Sing 'SchemaNull) where _ == _ = True
instance Eq (Sing ('SchemaArray as s)) where _ == _ = True
instance Eq (Sing ('SchemaObject cs)) where _ == _ = True
instance Eq (Sing ('SchemaOptional s)) where _ == _ = True

data FieldRepr :: (Symbol, Schema) -> Type where
  FieldRepr :: KnownSymbol name => JsonRepr schema -> FieldRepr '(name, schema)

type family MapSnd (cs :: [(Symbol, Schema)]) :: [Schema] where
  MapSnd '[] = '[]
  MapSnd ( '(fn, schema) ': as) = schema ': MapSnd as

deriving instance Show (JsonRepr schema) => Show (FieldRepr '(name, schema))

instance Eq (JsonRepr schema) => Eq (FieldRepr '(name, schema)) where
  FieldRepr a == FieldRepr b = a == b

instance
  ( KnownSymbol name
  , Serial m (JsonRepr schema) )
  => Serial m (FieldRepr '(name, schema)) where
  series = FieldRepr <$> series

data JsonRepr :: Schema -> Type where
  ReprText :: Text -> JsonRepr ('SchemaText cs)
  ReprNumber :: Scientific -> JsonRepr ('SchemaNumber cs)
  ReprNull :: JsonRepr 'SchemaNull
  ReprArray :: V.Vector (JsonRepr s) -> JsonRepr ('SchemaArray cs s)
  ReprObject :: Rec FieldRepr fs -> JsonRepr ('SchemaObject fs)
  ReprOptional :: Maybe (JsonRepr s) -> JsonRepr ('SchemaOptional s)

instance Show (JsonRepr ('SchemaText cs)) where
  show (ReprText t) = "ReprText " P.++ show t

instance Show (JsonRepr ('SchemaNumber cs)) where
  show (ReprNumber n) = "ReprNumber " P.++ show n

instance Show (JsonRepr 'SchemaNull) where show _ = "ReprNull"

instance Show (JsonRepr s) => Show (JsonRepr ('SchemaArray acs s)) where
  show (ReprArray v) = "ReprArray " P.++ show v

instance RecAll FieldRepr fs Show => Show (JsonRepr ('SchemaObject fs)) where
  show (ReprObject fs) = "ReprObject " P.++ show fs

instance Show (JsonRepr s) => Show (JsonRepr ('SchemaOptional s)) where
  show (ReprOptional s) = "ReprOptional " P.++ show s

instance (Monad m, Serial m Text)
  => Serial m (JsonRepr ('SchemaText cs)) where
  series = cons1 ReprText

instance (Monad m, Serial m Scientific)
  => Serial m (JsonRepr ('SchemaNumber cs)) where
  series = cons1 ReprNumber

instance Monad m => Serial m (JsonRepr 'SchemaNull) where
  series = cons0 ReprNull

instance (Serial m (V.Vector (JsonRepr s)))
  => Serial m (JsonRepr ('SchemaArray cs s)) where
  series = cons1 ReprArray

instance (Serial m (JsonRepr s))
  => Serial m (JsonRepr ('SchemaOptional s)) where
  series = cons1 ReprOptional

instance (Monad m, Serial m (Rec FieldRepr fs))
  => Serial m (JsonRepr ('SchemaObject fs)) where
  series = cons1 ReprObject

instance Eq (Rec FieldRepr fs) => Eq (JsonRepr ('SchemaObject fs)) where
  ReprObject a == ReprObject b = a == b

instance Eq (JsonRepr ('SchemaText cs)) where
  ReprText a == ReprText b = a == b

instance Eq (JsonRepr ('SchemaNumber cs)) where
  ReprNumber a == ReprNumber b = a == b

instance Eq (JsonRepr 'SchemaNull) where
  ReprNull == ReprNull = True

instance Eq (JsonRepr s) => Eq (JsonRepr ('SchemaArray as s)) where
  ReprArray a == ReprArray b = a == b

instance Eq (JsonRepr s) => Eq (JsonRepr ('SchemaOptional s)) where
  ReprOptional a == ReprOptional b = a == b

fromOptional
  :: (Known (Sing s))
  => Sing ('SchemaOptional s)
  -> J.Value
  -> Parser (Maybe (JsonRepr s))
fromOptional _ = parseJSON

instance Known (Sing schema) => J.FromJSON (JsonRepr schema) where
  parseJSON value = case known :: Sing schema of
    SSchemaText _        -> withText "String" (pure . ReprText) value
    SSchemaNumber _      -> withScientific "Number" (pure . ReprNumber) value
    SSchemaNull          -> case value of
      J.Null -> pure ReprNull
      _      -> typeMismatch "Null" value
    so@(SSchemaOptional _) -> ReprOptional <$> fromOptional so value
    SSchemaArray _ _     -> withArray "Array" (fmap ReprArray . traverse parseJSON) value
    SSchemaObject fs     -> do
      let
        demoteFields :: SList s -> H.HashMap Text J.Value -> Parser (Rec FieldRepr s)
        demoteFields SNil _ = pure RNil
        demoteFields (SCons (STuple2 (n :: Sing fn) s) tl) h = withKnownSymbol n $ do
          let fieldName = T.pack $ symbolVal (Proxy @fn)
          fieldRepr <- case s of
            SSchemaText _ -> case H.lookup fieldName h of
              Just v  -> FieldRepr <$> parseJSON v
              Nothing -> fail "schematext"
            SSchemaNumber _ -> case H.lookup fieldName h of
              Just v  -> FieldRepr <$> parseJSON v
              Nothing -> fail "schemanumber"
            SSchemaNull -> case H.lookup fieldName h of
              Just v  -> FieldRepr <$> parseJSON v
              Nothing -> fail "schemanull"
            SSchemaArray _ _ -> case H.lookup fieldName h of
              Just v  -> FieldRepr <$> parseJSON v
              Nothing -> fail "schemaarray"
            SSchemaObject _ -> case H.lookup fieldName h of
              Just v  -> FieldRepr <$> parseJSON v
              Nothing -> fail "schemaobject"
            SSchemaOptional _ -> case H.lookup fieldName h of
              Just v -> FieldRepr <$> parseJSON v
              Nothing -> fail "schemaoptional"
          (fieldRepr :&) <$> demoteFields tl h
      ReprObject <$> withObject "Object" (demoteFields fs) value

instance J.ToJSON (JsonRepr a) where
  toJSON ReprNull         = J.Null
  toJSON (ReprText t)     = J.String t
  toJSON (ReprNumber n)   = J.Number n
  toJSON (ReprOptional s) = case s of
    Just v -> toJSON v
    Nothing -> J.Null
  toJSON (ReprArray v)    = J.Array $ toJSON <$> v
  toJSON (ReprObject r)   = J.Object . H.fromList . fold $ r
    where
      extract :: forall name s . (KnownSymbol name)
        => FieldRepr '(name, s)
        -> (Text, Value)
      extract (FieldRepr s) = (T.pack $ symbolVal $ Proxy @name, toJSON s)
      fold :: Rec FieldRepr fs -> [(Text, J.Value)]
      fold = \case
        RNil                   -> []
        fr@(FieldRepr _) :& tl -> (extract fr) : fold tl

class FalseConstraint

type family TopLevel (spec :: Schema) :: Constraint where
  TopLevel ('SchemaArray a e) = ()
  TopLevel ('SchemaObject o)  = ()
  TopLevel spec              = FalseConstraint
