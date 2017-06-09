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

import Data.Scientific
import Data.Singletons.Prelude.List hiding (All)
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Text as T
import Data.Vector as V
import Data.Vinyl hiding (Dict)
import Data.Vinyl.TypeLevel hiding (Nat)
import GHC.Generics (Generic)
import Prelude as P
import Test.SmallCheck.Series


type family All (c :: k -> Constraint) (s :: [k]) :: Constraint where
  All c '[]       = ()
  All c (a ': as) = (c a, All c as)

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
  | TEnum [Symbol]
  deriving (Generic)

data instance Sing (tc :: TextConstraint) where
  STEq :: (KnownNat n) => Sing n -> Sing ('TEq n)
  STLe :: (KnownNat n) => Sing n -> Sing ('TLe n)
  STGt :: (KnownNat n) => Sing n -> Sing ('TGt n)
  STRegex :: (KnownSymbol s, SingI s) => Sing s -> Sing ('TRegex s)
  STEnum :: (All KnownSymbol ss, SingI ss) => Sing ss -> Sing ('TEnum ss)

instance (KnownNat n) => SingI ('TEq n) where sing = STEq sing
instance (KnownNat n) => SingI ('TGt n) where sing = STGt sing
instance (KnownNat n) => SingI ('TLe n) where sing = STLe sing
instance (KnownSymbol s, SingI s) => SingI ('TRegex s) where sing = STRegex sing
instance (All KnownSymbol ss, SingI ss) => SingI ('TEnum ss) where sing = STEnum sing

instance Eq (Sing ('TEq n)) where _ == _ = True
instance Eq (Sing ('TLe n)) where _ == _ = True
instance Eq (Sing ('TGt n)) where _ == _ = True
instance Eq (Sing ('TRegex t)) where _ == _ = True
instance Eq (Sing ('TEnum ss)) where _ == _ = True

data NumberConstraint
  = NLe Nat
  | NGt Nat
  | NEq Nat
  deriving (Generic)

data instance Sing (nc :: NumberConstraint) where
  SNEq :: KnownNat n => Sing n -> Sing ('NEq n)
  SNGt :: KnownNat n => Sing n -> Sing ('NGt n)
  SNLe :: KnownNat n => Sing n -> Sing ('NLe n)

instance KnownNat n => SingI ('NEq n) where sing = SNEq sing
instance KnownNat n => SingI ('NGt n) where sing = SNGt sing
instance KnownNat n => SingI ('NLe n) where sing = SNLe sing

instance Eq (Sing ('NEq n)) where _ == _ = True
instance Eq (Sing ('NLe n)) where _ == _ = True
instance Eq (Sing ('NGt n)) where _ == _ = True

data ArrayConstraint
  = AEq Nat
  deriving (Generic)

data instance Sing (ac :: ArrayConstraint) where
  SAEq :: KnownNat n => Sing n -> Sing ('AEq n)

instance KnownNat n => SingI ('AEq n) where sing = SAEq sing

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
  SSchemaText :: SingI tcs => Sing tcs -> Sing ('SchemaText tcs)
  SSchemaNumber :: SingI ncs => Sing ncs -> Sing ('SchemaNumber ncs)
  SSchemaArray :: (SingI acs, SingI schema) => Sing acs -> Sing schema -> Sing ('SchemaArray acs schema)
  SSchemaObject :: SingI fields => Sing fields -> Sing ('SchemaObject fields)
  SSchemaOptional :: SingI s => Sing s -> Sing ('SchemaOptional s)
  SSchemaNull :: Sing 'SchemaNull

instance SingI sl => SingI ('SchemaText sl) where
  sing = SSchemaText sing
instance SingI sl => SingI ('SchemaNumber sl) where
  sing = SSchemaNumber sing
instance SingI 'SchemaNull where
  sing = SSchemaNull
instance (SingI ac, SingI s) => SingI ('SchemaArray ac s) where
  sing = SSchemaArray sing sing
instance SingI stl => SingI ('SchemaObject stl) where
  sing = SSchemaObject sing
instance SingI s => SingI ('SchemaOptional s) where
  sing = SSchemaOptional sing

instance Eq (Sing ('SchemaText cs)) where _ == _ = True
instance Eq (Sing ('SchemaNumber cs)) where _ == _ = True
instance Eq (Sing 'SchemaNull) where _ == _ = True
instance Eq (Sing ('SchemaArray as s)) where _ == _ = True
instance Eq (Sing ('SchemaObject cs)) where _ == _ = True
instance Eq (Sing ('SchemaOptional s)) where _ == _ = True

data FieldRepr :: (Symbol, Schema) -> Type where
  FieldRepr
    :: (SingI schema, KnownSymbol name)
    => JsonRepr schema
    -> FieldRepr '(name, schema)

knownFieldName
  :: forall proxy (fieldName :: Symbol) schema
  .  KnownSymbol fieldName
  => proxy '(fieldName, schema)
  -> Text
knownFieldName _ = T.pack $ symbolVal (Proxy @fieldName)

knownFieldSchema
  :: forall proxy fieldName schema
  .  SingI schema
  => proxy '(fieldName, schema)
  -> Sing schema
knownFieldSchema _ = sing

deriving instance Show (JsonRepr schema) => Show (FieldRepr '(name, schema))

instance Eq (JsonRepr schema) => Eq (FieldRepr '(name, schema)) where
  FieldRepr a == FieldRepr b = a == b

instance
  ( KnownSymbol name
  , SingI schema
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
  :: SingI s
  => Sing ('SchemaOptional s)
  -> J.Value
  -> Parser (Maybe (JsonRepr s))
fromOptional _ = parseJSON

instance SingI schema => J.FromJSON (JsonRepr schema) where
  parseJSON value = case sing :: Sing schema of
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

class FalseConstraint a

type family TopLevel (schema :: Schema) :: Constraint where
  TopLevel ('SchemaArray acs s) = ()
  TopLevel ('SchemaObject o)    = ()
  TopLevel spec                 = 'True ~ 'False
