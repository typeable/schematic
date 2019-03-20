{-# LANGUAGE CPP       #-}
{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module Data.Schematic.Schema where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Data.Aeson as J
import           Data.Aeson.Types as J
import           Data.HashMap.Strict as H
import           Data.Kind
import           Data.Maybe
import           Data.Schematic.Constraints
import           Data.Schematic.Generator
import           Data.Schematic.Generator.Regex
import           Data.Schematic.Instances ()
import           Data.Schematic.Verifier.Array
import           Data.Scientific
import           Data.Singletons.Prelude.List hiding (All, Union)
import           Data.Singletons.TH
import           Data.Singletons.TypeLits
import           Data.Text as T
import           Data.Union
import           Data.Vector as V
import           Data.Vinyl hiding (Dict)
import qualified Data.Vinyl.TypeLevel as V
import           GHC.Exts
import           GHC.Generics (Generic)
import           GHC.Natural
import           Prelude as P
import           Test.SmallCheck.Series as S


singletons [d|
  data Schema' s n
    = SchemaText [TextConstraint' s n]
    | SchemaBoolean
    | SchemaNumber [NumberConstraint' n]
    | SchemaObject [(s, Schema' s n)]
    | SchemaArray [ArrayConstraint' n] (Schema' s n)
    | SchemaNull
    | SchemaOptional (Schema' s n)
    | SchemaUnion [Schema' s n]
    deriving (Eq, Show, Ord, Generic)
  |]

type SchemaT = Schema' Text Natural
type Schema = Schema' Symbol Nat

type family CRepr (s :: Schema) :: Type where
  CRepr ('SchemaText cs)  = TextConstraintT
  CRepr ('SchemaNumber cs) = NumberConstraintT
  CRepr ('SchemaObject fs) = (String, SchemaT)
  CRepr ('SchemaArray ar s) = ArrayConstraintT

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

type family USubsets (u :: [k]) :: Constraint where
  USubsets '[] = ()
  USubsets (h ': tl) = (USubset tl (h ': tl) (V.RImage tl (h ': tl)), USubsets tl)

data JsonRepr :: Schema -> Type where
  ReprText :: Text -> JsonRepr ('SchemaText cs)
  ReprNumber :: Scientific -> JsonRepr ('SchemaNumber cs)
  ReprBoolean :: Bool -> JsonRepr 'SchemaBoolean
  ReprNull :: JsonRepr 'SchemaNull
  ReprArray :: V.Vector (JsonRepr s) -> JsonRepr ('SchemaArray cs s)
  ReprObject :: Rec FieldRepr fs -> JsonRepr ('SchemaObject fs)
  ReprOptional :: Maybe (JsonRepr s) -> JsonRepr ('SchemaOptional s)
  ReprUnion :: Union JsonRepr (h ': tl) -> JsonRepr ('SchemaUnion (h ': tl))

instance (Monad m, Serial m Text, SingI cs)
  => Serial m (JsonRepr ('SchemaText cs)) where
  series = decDepth $ fmap ReprText $ textSeries $ fromSing (sing :: Sing cs)

instance (Monad m, Serial m Scientific, SingI cs)
  => Serial m (JsonRepr ('SchemaNumber cs)) where
  series = decDepth $ fmap ReprNumber
    $ numberSeries $ fromSing (sing :: Sing cs)

instance Monad m => Serial m (JsonRepr 'SchemaNull) where
  series = cons0 ReprNull

arraySeries
  :: (Monad m, Serial m (JsonRepr s))
  => [ArrayConstraintT] -> S.Series m (V.Vector (JsonRepr s))
arraySeries cs = maybe (pure V.empty) arraySeries' $ verifyArrayConstraint cs

arraySeries'
  :: forall m s. (Monad m, Serial m (JsonRepr s))
  => Maybe VerifiedArrayConstraint -> S.Series m (V.Vector (JsonRepr s))
arraySeries' ml =
  V.replicateM (maybe minRepeat f ml) (series :: S.Series m (JsonRepr s))
  where
    f (VAEq l) = fromIntegral l

instance (Serial m (JsonRepr s), Serial m (V.Vector (JsonRepr s)), SingI cs)
  => Serial m (JsonRepr ('SchemaArray cs s)) where
  series = decDepth $ fmap ReprArray
    $ arraySeries $ fromSing (sing :: Sing cs)

instance (Serial m (JsonRepr s))
  => Serial m (JsonRepr ('SchemaOptional s)) where
  series = cons1 ReprOptional

instance (Monad m, Serial m (Rec FieldRepr fs))
  => Serial m (JsonRepr ('SchemaObject fs)) where
  series = cons1 ReprObject

-- | Move to the union package
instance Show (JsonRepr ('SchemaText cs)) where
  show (ReprText t) = "ReprText " P.++ show t

instance Show (JsonRepr ('SchemaNumber cs)) where
  show (ReprNumber n) = "ReprNumber " P.++ show n

instance Show (JsonRepr 'SchemaBoolean) where
  show (ReprBoolean n) = "ReprBoolean " P.++ show n

instance Show (JsonRepr 'SchemaNull) where show _ = "ReprNull"

instance Show (JsonRepr s) => Show (JsonRepr ('SchemaArray acs s)) where
  show (ReprArray v) = "ReprArray " P.++ show v

#if MIN_VERSION_base(4,12,0)
instance
  ( V.RecAll FieldRepr fs Show, RMap fs, ReifyConstraint Show FieldRepr fs
  , RecordToList fs )
  => Show (JsonRepr ('SchemaObject fs)) where
  show (ReprObject fs) = "ReprObject " P.++ show fs
#else
instance V.RecAll FieldRepr fs Show => Show (JsonRepr ('SchemaObject fs)) where
  show (ReprObject fs) = "ReprObject " P.++ show fs
#endif

instance Show (JsonRepr s) => Show (JsonRepr ('SchemaOptional s)) where
  show (ReprOptional s) = "ReprOptional " P.++ show s

instance Show (Union JsonRepr (h ': tl))
  => Show (JsonRepr ('SchemaUnion (h ': tl))) where
  show (ReprUnion s) = "ReprUnion " P.++ show s

instance Eq (Rec FieldRepr fs) => Eq (JsonRepr ('SchemaObject fs)) where
  ReprObject a == ReprObject b = a == b

instance Eq (JsonRepr ('SchemaText cs)) where
  ReprText a == ReprText b = a == b

instance Eq (JsonRepr ('SchemaNumber cs)) where
  ReprNumber a == ReprNumber b = a == b

instance Eq (JsonRepr 'SchemaBoolean) where
  ReprBoolean a == ReprBoolean b = a == b

instance Eq (JsonRepr 'SchemaNull) where
  ReprNull == ReprNull = True

instance Eq (JsonRepr s) => Eq (JsonRepr ('SchemaArray as s)) where
  ReprArray a == ReprArray b = a == b

instance Eq (JsonRepr s) => Eq (JsonRepr ('SchemaOptional s)) where
  ReprOptional a == ReprOptional b = a == b

instance Eq (Union JsonRepr (h ': tl))
  => Eq (JsonRepr ('SchemaUnion (h ': tl))) where
  ReprUnion a == ReprUnion b = a == b

instance Ord (Rec FieldRepr fs) => Ord (JsonRepr ('SchemaObject fs)) where
  ReprObject a `compare` ReprObject b = a `compare` b

instance Ord (JsonRepr ('SchemaText cs)) where
  ReprText a `compare` ReprText b = a `compare` b

instance Ord (JsonRepr ('SchemaNumber cs)) where
  ReprNumber a `compare` ReprNumber b = a `compare` b

instance Ord (JsonRepr 'SchemaBoolean) where
  ReprBoolean a `compare` ReprBoolean b = a `compare` b

instance Ord (JsonRepr 'SchemaNull) where
  compare _ _ = EQ

instance Ord (JsonRepr s) => Ord (JsonRepr ('SchemaArray as s)) where
  ReprArray a `compare` ReprArray b = a `compare` b

instance Ord (JsonRepr s) => Ord (JsonRepr ('SchemaOptional s)) where
  ReprOptional a `compare` ReprOptional b = a `compare` b

instance Ord (Union JsonRepr (h ': tl))
  => Ord (JsonRepr ('SchemaUnion (h ': tl))) where
  ReprUnion a `compare` ReprUnion b = a `compare` b

instance IsList (JsonRepr ('SchemaArray cs s)) where
  type Item (JsonRepr ('SchemaArray cs s)) = JsonRepr s
  fromList = ReprArray . GHC.Exts.fromList
  toList (ReprArray v) = GHC.Exts.toList v

instance Num (JsonRepr ('SchemaNumber cs)) where
  ReprNumber a + ReprNumber b = ReprNumber $ a + b
  ReprNumber a - ReprNumber b = ReprNumber $ a - b
  ReprNumber a * ReprNumber b = ReprNumber $ a * b
  abs (ReprNumber a) = ReprNumber $ abs a
  signum (ReprNumber a) = ReprNumber $ signum a
  fromInteger = ReprNumber . fromIntegral

instance IsString (JsonRepr ('SchemaText cs)) where
  fromString = ReprText . fromString

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
              Nothing -> fail $ "No text field: " P.++ show fieldName
            SSchemaNumber so -> case H.lookup fieldName h of
              Just v  -> withSingI so $ FieldRepr <$> parseJSON v
              Nothing -> fail $ "No number field: " P.++ show fieldName
            SSchemaBoolean -> case H.lookup fieldName h of
              Just v  -> FieldRepr <$> parseJSON v
              Nothing -> fail $ "No boolean field: " P.++ show fieldName
            SSchemaNull -> case H.lookup fieldName h of
              Just v  -> FieldRepr <$> parseJSON v
              Nothing -> fail $ "No null field: " P.++ show fieldName
            SSchemaArray sa sb -> case H.lookup fieldName h of
              Just v  -> withSingI sa $ withSingI sb $ FieldRepr <$> parseJSON v
              Nothing -> fail $ "No array field: " P.++ show fieldName
            SSchemaObject so -> case H.lookup fieldName h of
              Just v  -> withSingI so $ FieldRepr <$> parseJSON v
              Nothing -> fail $ "No object field" P.++ show fieldName
            SSchemaOptional so -> case H.lookup fieldName h of
              Just v -> withSingI so $ FieldRepr <$> parseJSON v
              Nothing -> withSingI so $ pure $ FieldRepr $ ReprOptional Nothing
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
    Just v  -> toJSON v
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
