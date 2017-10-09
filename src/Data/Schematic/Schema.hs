{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module Data.Schematic.Schema where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Data.Aeson as J
import           Data.Aeson.Types as J
import           Data.HashMap.Strict as H
import           Data.Kind
import           Data.Maybe
import           Data.Schematic.Instances ()
import           Data.Scientific
import           Data.Singletons.Prelude.List hiding (All, Union)
import           Data.Singletons.TH
import           Data.Singletons.TypeLits
import           Data.Text as T
import           Data.Union
import           Data.Vector as V
import           Data.Vinyl hiding (Dict)
import qualified Data.Vinyl.TypeLevel as V
import           GHC.Generics (Generic)
import           GHC.TypeLits (SomeNat(..), SomeSymbol(..), someSymbolVal, someNatVal)
import           Prelude as P
import           Test.SmallCheck.Series


type family CRepr (s :: Schema) :: Type where
  CRepr ('SchemaText cs)  = TextConstraint
  CRepr ('SchemaNumber cs) = NumberConstraint
  CRepr ('SchemaObject fs) = (String, Schema)
  CRepr ('SchemaArray ar s) = ArrayConstraint

data TextConstraint
  = TEq Nat
  | TLt Nat
  | TLe Nat
  | TGt Nat
  | TGe Nat
  | TRegex Symbol
  | TEnum [Symbol]
  deriving (Generic)

instance SingKind TextConstraint where
  type Demote TextConstraint = DemotedTextConstraint
  fromSing = \case
    STEq n -> withKnownNat n (DTEq $ natVal n)
    STLt n -> withKnownNat n (DTLt $ natVal n)
    STLe n -> withKnownNat n (DTLe $ natVal n)
    STGt n -> withKnownNat n (DTGt $ natVal n)
    STGe n -> withKnownNat n (DTGe $ natVal n)
    STRegex s -> withKnownSymbol s (DTRegex $ T.pack $ symbolVal s)
    STEnum s -> let
      d :: Sing (s :: [Symbol]) -> [Text]
      d SNil              = []
      d (SCons ss@SSym fs) = T.pack (symbolVal ss) : d fs
      in DTEnum $ d s
  toSing = \case
    DTEq n -> case someNatVal n of
      Just (SomeNat (_ :: Proxy n)) -> SomeSing (STEq (SNat :: Sing n))
      Nothing -> error "Negative singleton nat"
    DTLt n -> case someNatVal n of
      Just (SomeNat (_ :: Proxy n)) -> SomeSing (STLt (SNat :: Sing n))
      Nothing -> error "Negative singleton nat"
    DTLe n -> case someNatVal n of
      Just (SomeNat (_ :: Proxy n)) -> SomeSing (STLe (SNat :: Sing n))
      Nothing -> error "Negative singleton nat"
    DTGt n -> case someNatVal n of
      Just (SomeNat (_ :: Proxy n)) -> SomeSing (STGt (SNat :: Sing n))
      Nothing -> error "Negative singleton nat"
    DTGe n -> case someNatVal n of
      Just (SomeNat (_ :: Proxy n)) -> SomeSing (STGe (SNat :: Sing n))
      Nothing -> error "Negative singleton nat"
    DTRegex s -> case someSymbolVal (T.unpack s) of
      SomeSymbol (_ :: Proxy n) -> SomeSing (STRegex (SSym :: Sing n))
    DTEnum ss -> case toSing ss of
      SomeSing l -> SomeSing (STEnum l)

data DemotedTextConstraint
  = DTEq Integer
  | DTLt Integer
  | DTLe Integer
  | DTGt Integer
  | DTGe Integer
  | DTRegex Text
  | DTEnum [Text]
  deriving (Generic)

data instance Sing (tc :: TextConstraint) where
  STEq :: Sing n -> Sing ('TEq n)
  STLt :: Sing n -> Sing ('TLt n)
  STLe :: Sing n -> Sing ('TLe n)
  STGt :: Sing n -> Sing ('TGt n)
  STGe :: Sing n -> Sing ('TGe n)
  STRegex :: Sing s -> Sing ('TRegex s)
  STEnum :: Sing ss -> Sing ('TEnum ss)

instance (KnownNat n) => SingI ('TEq n) where sing = STEq sing
instance (KnownNat n) => SingI ('TGt n) where sing = STGt sing
instance (KnownNat n) => SingI ('TGe n) where sing = STGe sing
instance (KnownNat n) => SingI ('TLt n) where sing = STLt sing
instance (KnownNat n) => SingI ('TLe n) where sing = STLe sing
instance (KnownSymbol s, SingI s) => SingI ('TRegex s) where sing = STRegex sing
instance (SingI ss) => SingI ('TEnum ss) where sing = STEnum sing

instance Eq (Sing ('TEq n)) where _ == _ = True
instance Eq (Sing ('TLt n)) where _ == _ = True
instance Eq (Sing ('TLe n)) where _ == _ = True
instance Eq (Sing ('TGt n)) where _ == _ = True
instance Eq (Sing ('TGe n)) where _ == _ = True
instance Eq (Sing ('TRegex t)) where _ == _ = True
instance Eq (Sing ('TEnum ss)) where _ == _ = True

data NumberConstraint
  = NLe Nat
  | NLt Nat
  | NGt Nat
  | NGe Nat
  | NEq Nat
  deriving (Generic)

data DemotedNumberConstraint
  = DNLe Integer
  | DNLt Integer
  | DNGt Integer
  | DNGe Integer
  | DNEq Integer
  deriving (Generic)

data instance Sing (nc :: NumberConstraint) where
  SNEq :: Sing n -> Sing ('NEq n)
  SNGt :: Sing n -> Sing ('NGt n)
  SNGe :: Sing n -> Sing ('NGe n)
  SNLt :: Sing n -> Sing ('NLt n)
  SNLe :: Sing n -> Sing ('NLe n)

instance KnownNat n => SingI ('NEq n) where sing = SNEq sing
instance KnownNat n => SingI ('NGt n) where sing = SNGt sing
instance KnownNat n => SingI ('NGe n) where sing = SNGe sing
instance KnownNat n => SingI ('NLt n) where sing = SNLt sing
instance KnownNat n => SingI ('NLe n) where sing = SNLe sing

instance Eq (Sing ('NEq n)) where _ == _ = True
instance Eq (Sing ('NLt n)) where _ == _ = True
instance Eq (Sing ('NLe n)) where _ == _ = True
instance Eq (Sing ('NGt n)) where _ == _ = True
instance Eq (Sing ('NGe n)) where _ == _ = True

instance SingKind NumberConstraint where
  type Demote NumberConstraint = DemotedNumberConstraint
  fromSing = \case
    SNEq n -> withKnownNat n (DNEq $ natVal n)
    SNGt n -> withKnownNat n (DNGt $ natVal n)
    SNGe n -> withKnownNat n (DNGe $ natVal n)
    SNLt n -> withKnownNat n (DNLt $ natVal n)
    SNLe n -> withKnownNat n (DNLe $ natVal n)
  toSing = \case
    DNEq n -> case someNatVal n of
      Just (SomeNat (_ :: Proxy n)) -> SomeSing (SNEq (SNat :: Sing n))
      Nothing -> error "Negative singleton nat"
    DNGt n -> case someNatVal n of
      Just (SomeNat (_ :: Proxy n)) -> SomeSing (SNGt (SNat :: Sing n))
      Nothing -> error "Negative singleton nat"
    DNGe n -> case someNatVal n of
      Just (SomeNat (_ :: Proxy n)) -> SomeSing (SNGe (SNat :: Sing n))
      Nothing -> error "Negative singleton nat"
    DNLt n -> case someNatVal n of
      Just (SomeNat (_ :: Proxy n)) -> SomeSing (SNLt (SNat :: Sing n))
      Nothing -> error "Negative singleton nat"
    DNLe n -> case someNatVal n of
      Just (SomeNat (_ :: Proxy n)) -> SomeSing (SNLe (SNat :: Sing n))
      Nothing -> error "Negative singleton nat"

data ArrayConstraint
  = AEq Nat
  deriving (Generic)

data DemotedArrayConstraint
  = DAEq Integer
  deriving (Generic)

data instance Sing (ac :: ArrayConstraint) where
  SAEq :: Sing n -> Sing ('AEq n)

instance KnownNat n => SingI ('AEq n) where sing = SAEq sing

instance Eq (Sing ('AEq n)) where _ == _ = True

instance SingKind ArrayConstraint where
  type Demote ArrayConstraint = DemotedArrayConstraint
  fromSing = \case
    SAEq n -> withKnownNat n (DAEq $ natVal n)
  toSing = \case
    DAEq n -> case someNatVal n of
      Just (SomeNat (_ :: Proxy n)) -> SomeSing (SAEq (SNat :: Sing n))
      Nothing -> error "Negative singleton nat"

data Schema
  = SchemaText [TextConstraint]
  | SchemaBoolean
  | SchemaNumber [NumberConstraint]
  | SchemaObject [(Symbol, Schema)]
  | SchemaArray [ArrayConstraint] Schema
  | SchemaNull
  | SchemaOptional Schema
  | SchemaUnion [Schema]
  deriving (Generic)

data DemotedSchema
  = DSchemaText [DemotedTextConstraint]
  | DSchemaNumber [DemotedNumberConstraint]
  | DSchemaBoolean
  | DSchemaObject [(Text, DemotedSchema)]
  | DSchemaArray [DemotedArrayConstraint] DemotedSchema
  | DSchemaNull
  | DSchemaOptional DemotedSchema
  | DSchemaUnion [DemotedSchema]
  deriving (Generic)

data instance Sing (schema :: Schema) where
  SSchemaText :: Sing tcs -> Sing ('SchemaText tcs)
  SSchemaNumber :: Sing ncs -> Sing ('SchemaNumber ncs)
  SSchemaBoolean :: Sing 'SchemaBoolean
  SSchemaArray :: Sing acs -> Sing schema -> Sing ('SchemaArray acs schema)
  SSchemaObject :: Sing fields -> Sing ('SchemaObject fields)
  SSchemaOptional :: Sing s -> Sing ('SchemaOptional s)
  SSchemaNull :: Sing 'SchemaNull
  SSchemaUnion :: Sing ss -> Sing ('SchemaUnion ss)

instance SingI sl => SingI ('SchemaText sl) where
  sing = SSchemaText sing
instance SingI sl => SingI ('SchemaNumber sl) where
  sing = SSchemaNumber sing
instance SingI 'SchemaNull where
  sing = SSchemaNull
instance SingI 'SchemaBoolean where
  sing = SSchemaBoolean
instance (SingI ac, SingI s) => SingI ('SchemaArray ac s) where
  sing = SSchemaArray sing sing
instance SingI stl => SingI ('SchemaObject stl) where
  sing = SSchemaObject sing
instance SingI s => SingI ('SchemaOptional s) where
  sing = SSchemaOptional sing
instance SingI s => SingI ('SchemaUnion s) where
  sing = SSchemaUnion sing

instance Eq (Sing ('SchemaText cs)) where _ == _ = True
instance Eq (Sing ('SchemaNumber cs)) where _ == _ = True
instance Eq (Sing 'SchemaNull) where _ == _ = True
instance Eq (Sing 'SchemaBoolean) where _ == _ = True
instance Eq (Sing ('SchemaArray as s)) where _ == _ = True
instance Eq (Sing ('SchemaObject cs)) where _ == _ = True
instance Eq (Sing ('SchemaOptional s)) where _ == _ = True
instance Eq (Sing ('SchemaUnion s)) where _ == _ = True

instance SingKind Schema where
  type Demote Schema = DemotedSchema
  fromSing = \case
    SSchemaText cs -> DSchemaText $ fromSing cs
    SSchemaNumber cs -> DSchemaNumber $ fromSing cs
    SSchemaBoolean -> DSchemaBoolean
    SSchemaArray cs s -> DSchemaArray (fromSing cs) (fromSing s)
    SSchemaOptional s -> DSchemaOptional $ fromSing s
    SSchemaNull -> DSchemaNull
    SSchemaObject cs -> DSchemaObject $ fromSing cs
    SSchemaUnion ss -> DSchemaUnion $ fromSing ss
  toSing = \case
    DSchemaText cs -> case toSing cs of
      SomeSing scs -> SomeSing $ SSchemaText scs
    DSchemaNumber cs -> case toSing cs of
      SomeSing scs -> SomeSing $ SSchemaNumber scs
    DSchemaBoolean -> SomeSing $ SSchemaBoolean
    DSchemaArray cs sch -> case (toSing cs, toSing sch) of
      (SomeSing scs, SomeSing ssch) -> SomeSing $ SSchemaArray scs ssch
    DSchemaOptional sch -> case toSing sch of
      SomeSing ssch -> SomeSing $ SSchemaOptional ssch
    DSchemaNull -> SomeSing SSchemaNull
    DSchemaObject cs -> case toSing cs of
      SomeSing scs -> SomeSing $ SSchemaObject scs
    DSchemaUnion ss -> case toSing ss of
      SomeSing sss -> SomeSing $ SSchemaUnion sss

data FieldRepr :: (Symbol, Schema) -> Type where
  FieldRepr
    :: (SingI schema, KnownSymbol name)
    => JsonRepr schema
    -> FieldRepr '(name, schema)

-- | Forgetful Functor Ufr
toJsonRepr :: FieldRepr '(fn, sch) -> JsonRepr sch
toJsonRepr (FieldRepr x) = x

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
  ReprBoolean :: Bool -> JsonRepr 'SchemaBoolean
  ReprNull :: JsonRepr 'SchemaNull
  ReprArray :: V.Vector (JsonRepr s) -> JsonRepr ('SchemaArray cs s)
  ReprObject :: Rec FieldRepr fs -> JsonRepr ('SchemaObject fs)
  ReprOptional :: Maybe (JsonRepr s) -> JsonRepr ('SchemaOptional s)
  ReprUnion :: Union JsonRepr (h ': tl) -> JsonRepr ('SchemaUnion (h ': tl))

-- | Move to the different package
type family UnionAll (f :: u -> *) (rs :: [u]) (c :: * -> Constraint) :: Constraint where
  UnionAll f '[] c = ()
  UnionAll f (r ': rs) c = (c (f r), UnionAll f rs c)

-- instance (UnionAll f as Show) => Show (Union f as) where
--   show (This fa) = "This " P.++ show fa
--   show (That u)  = "That " P.++ show u

instance Show (JsonRepr ('SchemaText cs)) where
  show (ReprText t) = "ReprText " P.++ show t

instance Show (JsonRepr ('SchemaNumber cs)) where
  show (ReprNumber n) = "ReprNumber " P.++ show n

instance Show (JsonRepr 'SchemaNull) where show _ = "ReprNull"

instance Show (JsonRepr s) => Show (JsonRepr ('SchemaArray acs s)) where
  show (ReprArray v) = "ReprArray " P.++ show v

instance V.RecAll FieldRepr fs Show => Show (JsonRepr ('SchemaObject fs)) where
  show (ReprObject fs) = "ReprObject " P.++ show fs

instance Show (JsonRepr s) => Show (JsonRepr ('SchemaOptional s)) where
  show (ReprOptional s) = "ReprOptional " P.++ show s

deriving instance UnionAll f as Show => Show (Union f as)

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

parseUnion
  :: FromJSON (JsonRepr ('SchemaUnion ss))
  => sing (ss :: [Schema])
  -> Value
  -> Parser (JsonRepr ('SchemaUnion ss))
parseUnion _ val = parseJSON val

instance FromJSON (Union JsonRepr '[]) where
  parseJSON = fail "empty union"

instance (SingI a, FromJSON (Union JsonRepr as)) => FromJSON (Union JsonRepr (a ': as)) where
  parseJSON val = (This <$> parseJSON val)
    <|> (That <$> (parseJSON val :: Parser (Union JsonRepr as)))

instance ToJSON (Union JsonRepr as) where
  toJSON (This fa) = toJSON fa
  toJSON (That u)  = toJSON u

instance SingI schema => J.FromJSON (JsonRepr schema) where
  parseJSON value = case sing :: Sing schema of
    SSchemaText _          -> withText "String" (pure . ReprText) value
    SSchemaNumber _        -> withScientific "Number" (pure . ReprNumber) value
    SSchemaBoolean         -> ReprBoolean <$> parseJSON value
    SSchemaNull            -> case value of
      J.Null -> pure ReprNull
      _      -> typeMismatch "Null" value
    so@(SSchemaOptional s) -> withSingI s $ ReprOptional <$> fromOptional so value
    SSchemaArray sa sb     -> withSingI sa $ withSingI sb
      $ withArray "Array" (fmap ReprArray . traverse parseJSON) value
    SSchemaObject fs       -> do
      let
        demoteFields :: SList s -> H.HashMap Text J.Value -> Parser (Rec FieldRepr s)
        demoteFields SNil _ = pure RNil
        demoteFields (SCons (STuple2 (n :: Sing fn) s) tl) h = withKnownSymbol n $ do
          let fieldName = T.pack $ symbolVal (Proxy @fn)
          fieldRepr <- case s of
            SSchemaText so -> case H.lookup fieldName h of
              Just v  -> withSingI so $ FieldRepr <$> parseJSON v
              Nothing -> fail "schematext"
            SSchemaNumber so -> case H.lookup fieldName h of
              Just v  -> withSingI so $ FieldRepr <$> parseJSON v
              Nothing -> fail "schemanumber"
            SSchemaBoolean -> case H.lookup fieldName h of
              Just v  -> FieldRepr <$> parseJSON v
              Nothing -> fail "schemaboolean"
            SSchemaNull -> case H.lookup fieldName h of
              Just v  -> FieldRepr <$> parseJSON v
              Nothing -> fail "schemanull"
            SSchemaArray sa sb -> case H.lookup fieldName h of
              Just v  -> withSingI sa $ withSingI sb $ FieldRepr <$> parseJSON v
              Nothing -> fail "schemaarray"
            SSchemaObject so -> case H.lookup fieldName h of
              Just v  -> withSingI so $ FieldRepr <$> parseJSON v
              Nothing -> fail "schemaobject"
            SSchemaOptional so -> case H.lookup fieldName h of
              Just v -> withSingI so $ FieldRepr <$> parseJSON v
              Nothing -> fail "schemaoptional"
            SSchemaUnion ss -> withSingI ss $ FieldRepr <$> parseUnion ss value
          (fieldRepr :&) <$> demoteFields tl h
      ReprObject <$> withObject "Object" (demoteFields fs) value
    SSchemaUnion ss -> parseUnion ss value

instance J.ToJSON (JsonRepr a) where
  toJSON ReprNull         = J.Null
  toJSON (ReprBoolean b)  = J.Bool b
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
  toJSON (ReprUnion u) = toJSON u

class FalseConstraint a

type family TopLevel (schema :: Schema) :: Constraint where
  TopLevel ('SchemaArray acs s) = ()
  TopLevel ('SchemaObject o)    = ()
  TopLevel spec                 = 'True ~ 'False
