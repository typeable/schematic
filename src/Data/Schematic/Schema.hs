{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE EmptyCase           #-}
-- {-# LANGUAGE PolyKinds           #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module Data.Schematic.Schema where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Data.Aeson as J
import           Data.Aeson.Types as J
import           Data.Char as C
import           Data.HashMap.Strict as H
import           Data.Kind
import           Data.Maybe
import           Data.Schematic.Compat
import           Data.Schematic.Constraints
import           Data.Schematic.Generator
import           Data.Schematic.Generator.Regex
import           Data.Schematic.Instances ()
import           Data.Schematic.Verifier.Array
import           Data.Scientific
import           Data.Singletons.TH
import           Data.Singletons.TypeLits
import           Data.Text as T
import           Data.Union
import           Data.Vector as V
import           Data.Vinyl hiding (Dict)
import           GHC.Exts
import           GHC.Generics (Generic)
import           Prelude as P
import           Test.SmallCheck.Series as S
#if !MIN_VERSION_base(4,12,0)
import qualified Data.Vinyl.TypeLevel as V
#endif
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid ((<>))
#endif


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
    deriving (Show, Generic)
  |]

type SchemaT = Schema' Text (Demote Nat)
type Schema = Schema' Symbol Nat

schemaTypeStr :: forall (sch :: Schema). SingI sch => String
schemaTypeStr =
  P.map C.toLower $ P.drop 6 $ P.head $ P.words $ show $ (demote' @sch)

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
deriving instance Eq (JsonRepr schema) => Eq (FieldRepr '(name, schema))
deriving instance Ord (JsonRepr schema) => Ord (FieldRepr '(name, schema))

instance
  ( KnownSymbol name
  , SingI schema
  , Serial m (JsonRepr schema) )
  => Serial m (FieldRepr '(name, schema)) where
  series = FieldRepr <$> series

#if MIN_VERSION_base(4,12,0)
type ReprObjectConstr fs =
  ( RMap fs, RecordToList fs, ReifyConstraint Show FieldRepr fs
  , Eq (Rec FieldRepr fs), Ord (Rec FieldRepr fs))
#else
type ReprObjectConstr fs =
  (V.RecAll FieldRepr fs Show, Eq (Rec FieldRepr fs))
#endif
type ReprUnionConstr tl =
  (Show (Union JsonRepr tl), Eq (Union JsonRepr tl), Ord (Union JsonRepr tl))

data JsonRepr :: Schema -> Type where
  ReprText :: Text -> JsonRepr ('SchemaText cs)
  ReprNumber :: Scientific -> JsonRepr ('SchemaNumber cs)
  ReprBoolean :: Bool -> JsonRepr 'SchemaBoolean
  ReprNull :: JsonRepr 'SchemaNull
  ReprArray :: V.Vector (JsonRepr s) -> JsonRepr ('SchemaArray cs s)
  ReprOptional :: Maybe (JsonRepr s) -> JsonRepr ('SchemaOptional s)
  ReprUnion :: ReprUnionConstr tl
    => Union JsonRepr (h ': tl) -> JsonRepr ('SchemaUnion (h ': tl))
  ReprObject :: ReprObjectConstr fs
    => Rec FieldRepr fs -> JsonRepr ('SchemaObject fs)

deriving instance Show (JsonRepr sch)
deriving instance Eq (JsonRepr sch)

-- due to issue https://gitlab.haskell.org/ghc/ghc/issues/8128
#if MIN_VERSION_base(4,12,0)
deriving instance Ord (JsonRepr sch)

#else
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
#endif

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

instance
  ( Monad m, Serial m (Rec FieldRepr fs), ReprObjectConstr fs)
  => Serial m (JsonRepr ('SchemaObject fs)) where
  series = cons1 ReprObject

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
  :: (SingI s, FromJSON (JsonRepr s))
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

instance (SingI a, FromJSON (JsonRepr a), FromJSON (Union JsonRepr as))
  => FromJSON (Union JsonRepr (a ': as)) where
  parseJSON val = (This <$> parseJSON val)
    <|> (That <$> (parseJSON val :: Parser (Union JsonRepr as)))

instance ToJSON (Union JsonRepr as) where
  toJSON (This fa) = toJSON fa
  toJSON (That u)  = toJSON u

instance J.FromJSON (JsonRepr ('SchemaText cs)) where
  parseJSON = withText "String" (pure . ReprText)

instance J.FromJSON (JsonRepr ('SchemaNumber cs)) where
  parseJSON = withScientific "Number" (pure . ReprNumber)

instance J.FromJSON (JsonRepr 'SchemaBoolean) where
  parseJSON = fmap ReprBoolean . parseJSON

instance J.FromJSON (JsonRepr 'SchemaNull) where
  parseJSON value = case value of
    J.Null -> pure ReprNull
    _      -> typeMismatch "Null" value

instance
  J.FromJSON (JsonRepr s) =>  J.FromJSON (JsonRepr ('SchemaOptional s)) where
  parseJSON = fmap ReprOptional . parseJSON

instance
  J.FromJSON (JsonRepr sb) => J.FromJSON (JsonRepr ('SchemaArray sa sb)) where
  parseJSON = withArray "Array" (fmap ReprArray . traverse parseJSON)

instance
  ( SingI x, ReprUnionConstr xs
  , FromJSON (Union JsonRepr xs), FromJSON (JsonRepr x) )
  => J.FromJSON (JsonRepr ('SchemaUnion (x ': xs))) where
  parseJSON = fmap ReprUnion . parseJSON

class FromHashMap (xs :: [(Symbol, Schema)]) where
  fromHashMap :: H.HashMap Text J.Value -> Parser (Rec FieldRepr xs)

instance FromHashMap '[] where
  fromHashMap _ = pure RNil

instance
  (KnownSymbol n, SingI s, FromHashMap xs, FromJSON (JsonRepr s))
  => FromHashMap ( '(n,s) ': xs) where
  fromHashMap h = do
    fr <- case H.lookup fn h of
      Nothing -> case (sing :: Sing s) of
        SSchemaOptional _ -> pure $ FieldRepr @s $ ReprOptional Nothing
        _ -> fail $ "No " <> schemaTypeStr @s <> " field: " <> show fn
      Just v -> FieldRepr @s <$> parseJSON v
    frs <- fromHashMap @xs h
    pure $ fr :& frs
    where
      fn = demote' @n

instance (FromHashMap rs, ReprObjectConstr rs)
  => J.FromJSON (JsonRepr ('SchemaObject rs)) where
  parseJSON = fmap ReprObject . withObject "Object" fromHashMap

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

-- class FalseConstraint a

type family TopLevel (schema :: Schema) :: Constraint where
  TopLevel ('SchemaArray acs s) = ()
  TopLevel ('SchemaObject o)    = ()
  TopLevel spec                 = 'True ~ 'False
