{-# OPTIONS -fprint-explicit-kinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Schematic
  ( module Data.Schematic.JsonSchema
  , module Data.Schematic.Helpers
  , module Data.Schematic.Lens
  , module Data.Schematic.Migration
  , module Data.Schematic.Schema
  , module Data.Schematic.Utils
  , decodeAndValidateJson
  , parseAndValidateJson
  , parseAndValidateJsonBy
  , parseAndValidateTopVersionJson
  , parseAndValidateWithMList
  , decodeAndValidateVersionedWithMList
  , decodeAndValidateVersionedWithPureMList
  , isValid
  , isDecodingError
  , isValidationError
  , ParseResult(..)
  , withRepr
  , field
  ) where

import           Control.Monad.Validation
import           Data.Aeson as J
import           Data.Aeson.Types as J
import           Data.ByteString.Lazy as BL
import           Data.Functor.Identity as F
import           Data.Kind
import           Data.Schematic.Helpers
import           Data.Schematic.JsonSchema
import           Data.Schematic.Lens
import           Data.Schematic.Migration
import           Data.Schematic.Schema
import           Data.Schematic.Utils
import           Data.Schematic.Validation
import           Data.Scientific
import           Data.Singletons.Prelude hiding ((:.))
import           Data.Singletons.TypeLits
import           Data.Tagged
import           Data.Text as T
import           Data.Union
import qualified Data.Vector as V
import           Data.Vinyl as V
import           Data.Vinyl.Functor


parseAndValidateJson
  :: forall schema
  .  (J.FromJSON (JsonRepr schema), TopLevel schema, SingI schema)
  => J.Value
  -> ParseResult (JsonRepr schema)
parseAndValidateJson v =
  case parseEither parseJSON v of
    Left s         -> DecodingError $ T.pack s
    Right jsonRepr ->
      let
        validate = validateJsonRepr (sing :: Sing schema) [] jsonRepr
        res      = runIdentity . runValidationTEither $ validate
      in case res of
        Left em  -> ValidationError em
        Right () -> Valid jsonRepr

parseAndValidateJsonBy
  :: (J.FromJSON (JsonRepr schema), TopLevel schema, SingI schema)
  => proxy schema
  -> J.Value
  -> ParseResult (JsonRepr schema)
parseAndValidateJsonBy _ = parseAndValidateJson

parseAndValidateTopVersionJson
  :: forall proxy (v :: Versioned)
  .  (SingI (TopVersion (AllVersions v)))
  => proxy v
  -> J.Value
  -> ParseResult (JsonRepr (TopVersion (AllVersions v)))
parseAndValidateTopVersionJson _ v =
  case parseEither parseJSON v of
    Left s -> DecodingError $ T.pack s
    Right jsonRepr ->
      let
        validate =
          validateJsonRepr (sing :: Sing (TopVersion (AllVersions v))) [] jsonRepr
        res      = runIdentity . runValidationTEither $ validate
      in case res of
        Left em  -> ValidationError em
        Right () -> Valid jsonRepr

parseAndValidateWithMList
  :: Monad m
  => MList m revisions
  -> J.Value
  -> m (ParseResult (JsonRepr (Head revisions)))
parseAndValidateWithMList MNil v = pure $ parseAndValidateJson v
parseAndValidateWithMList ((:&&) p f tl) v = case parseAndValidateJsonBy p v of
    Valid a           -> pure $ Valid a
    DecodingError _   -> do
      pr <- parseAndValidateWithMList tl v
      let pr' = f <$> pr
      sequence pr'
    ValidationError _ -> do
      pr <- parseAndValidateWithMList tl v
      let pr' = f <$> pr
      sequence pr'

decodeAndValidateJson
  :: forall schema
  .  (J.FromJSON (JsonRepr schema), TopLevel schema, SingI schema)
  => BL.ByteString
  -> ParseResult (JsonRepr schema)
decodeAndValidateJson bs = case decode bs of
  Nothing -> DecodingError "malformed json"
  Just x  -> parseAndValidateJson x

type family MapSnd (l :: [(a,k)]) = (r :: [k]) where
  MapSnd '[] = '[]
  MapSnd ( '(a, b) ': tl) = b ': MapSnd tl

decodeAndValidateVersionedWithMList
  :: Monad m
  => proxy versioned
  -> MList m (MapSnd (AllVersions versioned))
  -> BL.ByteString
  -> m (ParseResult (JsonRepr (Head (MapSnd (AllVersions versioned)))))
decodeAndValidateVersionedWithMList _ mlist bs = case decode bs of
  Nothing -> pure $ DecodingError "malformed json"
  Just x  -> parseAndValidateWithMList mlist x

decodeAndValidateVersionedWithPureMList
  :: proxy versioned
  -> MList F.Identity (MapSnd (AllVersions versioned))
  -> BL.ByteString
  -> ParseResult (JsonRepr (Head (MapSnd (AllVersions versioned))))
decodeAndValidateVersionedWithPureMList a b c =
  runIdentity $ decodeAndValidateVersionedWithMList a b c

-- convenience helpers

type Constructor a
  = forall b. FSubset (FieldsOf a) b (FImage (FieldsOf a) b)
  => Rec (Tagged (FieldsOf a) :. FieldRepr) b
  -> JsonRepr ('SchemaObject (FieldsOf a))

withRepr :: Constructor a
withRepr = ReprObject . rmap (unTagged . getCompose) . fcast

class Representable s where
  type Repr s :: Type
  construct :: Sing fn -> Proxy s -> Repr s -> FieldRepr '(fn, s)

instance SingI so => Representable ('SchemaObject so) where
  type Repr ('SchemaObject so) = Rec FieldRepr so
  construct sfn _ o = withKnownSymbol sfn $ FieldRepr $ ReprObject o

instance (SingI cs, SingI sa) => Representable ('SchemaArray cs sa) where
  type Repr ('SchemaArray cs sa) = V.Vector (JsonRepr sa)
  construct sfn _ a = withKnownSymbol sfn $ FieldRepr $ ReprArray a

instance SingI cs => Representable ('SchemaText cs) where
  type Repr ('SchemaText cs) = Text
  construct sfn _ t = withKnownSymbol sfn $ FieldRepr $ ReprText t

instance SingI cs => Representable ('SchemaNumber cs) where
  type Repr ('SchemaNumber cs) = Scientific
  construct sfn _ n = withKnownSymbol sfn $ FieldRepr $ ReprNumber n

instance Representable 'SchemaBoolean where
  type Repr 'SchemaBoolean = Bool
  construct sfn _ b = withKnownSymbol sfn $ FieldRepr $ ReprBoolean b

instance SingI so => Representable ('SchemaOptional so) where
  type Repr ('SchemaOptional so) = Maybe (JsonRepr so)
  construct sfn _ o = withKnownSymbol sfn $ FieldRepr $ ReprOptional o

instance SingI (h ': tl) => Representable ('SchemaUnion (h ': tl)) where
  type Repr ('SchemaUnion (h ': tl)) = Union JsonRepr (h ': tl)
  construct sfn _ u = withKnownSymbol sfn $ FieldRepr $ ReprUnion u

type family FieldsOf (s :: Schema) :: [(Symbol, Schema)] where
  FieldsOf ('SchemaObject fs) = fs

type FieldConstructor fn =
  forall fs. (Representable (ByField fn fs (FIndex fn fs)))
  => Repr (ByField fn fs (FIndex fn fs))
  -> (Tagged fs :. FieldRepr) '(fn, (ByField fn fs (FIndex fn fs)))

field :: forall fn. KnownSymbol fn => FieldConstructor fn
field = Compose . Tagged . construct (sing :: Sing fn) Proxy
