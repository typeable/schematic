module Data.Schematic.Validation where

import Control.Monad
import Control.Monad.Validation
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import Data.Functor.Identity
import Data.Monoid
import Data.Schematic.Path
import Data.Schematic.Schema
import Data.Scientific
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Data.Text as T
import Data.Traversable
import Data.Union
import Data.Vector as V
import Data.Vinyl
import Data.Vinyl.TypeLevel
import Prelude as P
import Text.Regex.TDFA


type Validation a = ValidationT ErrorMap Identity a

type ErrorMap = MonoidMap Text [Text]

data ParseResult a
  = Valid a
  | DecodingError Text       -- static
  | ValidationError ErrorMap -- runtime
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance (TopLevel a, SingI a, FromJSON (JsonRepr a))
  => FromJSON (ParseResult (JsonRepr a)) where
    parseJSON = pure . parseAndValidateJson @a

isValid :: ParseResult a -> Bool
isValid (Valid _) = True
isValid _ = False

isDecodingError :: ParseResult a -> Bool
isDecodingError (DecodingError _) = True
isDecodingError _                 = False

isValidationError :: ParseResult a -> Bool
isValidationError (ValidationError _) = True
isValidationError _                   = False

validateTextConstraint
  :: JSONPath
  -> Text
  -> Sing (tcs :: TextConstraint)
  -> Validation ()
validateTextConstraint (JSONPath path) t = \case
  STEq n -> do
    let
      nlen      = withKnownNat n $ natVal n
      predicate = nlen == (fromIntegral $ T.length t)
      errMsg    = "length of " <> path <> " should be == " <> T.pack (show nlen)
      warn      = vWarning $ mmSingleton path (pure errMsg)
    unless predicate warn
  STLt n -> do
    let
      nlen      = withKnownNat n $ natVal n
      predicate = nlen < (fromIntegral $ T.length t)
      errMsg    = "length of " <> path <> " should be < " <> T.pack (show nlen)
      warn      = vWarning $ mmSingleton path (pure errMsg)
    unless predicate warn
  STLe n -> do
    let
      nlen      = withKnownNat n $ natVal n
      predicate = nlen <= (fromIntegral $ T.length t)
      errMsg    = "length of " <> path <> " should be <= " <> T.pack (show nlen)
      warn      = vWarning $ mmSingleton path (pure errMsg)
    unless predicate warn
  STGt n -> do
    let
      nlen      = withKnownNat n $ natVal n
      predicate = nlen > (fromIntegral $ T.length t)
      errMsg    = "length of " <> path <> " should be > " <> T.pack (show nlen)
      warn      = vWarning $ mmSingleton path (pure errMsg)
    unless predicate warn
  STGe n -> do
    let
      nlen      = withKnownNat n $ natVal n
      predicate = nlen >= (fromIntegral $ T.length t)
      errMsg    = "length of " <> path <> " should be >= " <> T.pack (show nlen)
      warn      = vWarning $ mmSingleton path (pure errMsg)
    unless predicate warn
  STRegex r -> do
    let
      regex     = withKnownSymbol r $ fromSing r
      predicate = matchTest (makeRegex (T.unpack regex) :: Regex) (T.unpack t)
      errMsg    = path <> " must match " <> regex
      warn      = vWarning $ mmSingleton path (pure errMsg)
    unless predicate warn
  STEnum ss -> do
    let
      matching :: Sing (s :: [Symbol]) -> Bool
      matching SNil              = False
      matching (SCons s@SSym fs) = T.pack (symbolVal s) == t || matching fs
      errMsg = path <> " must be one of " <> T.pack (show (fromSing ss))
      warn   = vWarning $ mmSingleton path (pure errMsg)
    unless (matching ss) warn

validateNumberConstraint
  :: JSONPath
  -> Scientific
  -> Sing (tcs :: NumberConstraint)
  -> Validation ()
validateNumberConstraint (JSONPath path) num = \case
  SNEq n -> do
    let
      nlen      = withKnownNat n $ natVal n
      predicate = fromIntegral nlen == num
      errMsg    = path <> " should be == " <> T.pack (show nlen)
      warn      = vWarning $ mmSingleton path (pure errMsg)
    unless predicate warn
  SNGt n -> do
    let
      nlen      = withKnownNat n $ natVal n
      predicate = num > fromIntegral nlen
      errMsg    = path <> " should be > " <> T.pack (show nlen)
      warn      = vWarning $ mmSingleton path (pure errMsg)
    unless predicate warn
  SNGe n -> do
    let
      nlen      = withKnownNat n $ natVal n
      predicate = num >= fromIntegral nlen
      errMsg    = path <> " should be >= " <> T.pack (show nlen)
      warn      = vWarning $ mmSingleton path (pure errMsg)
    unless predicate warn
  SNLt n -> do
    let
      nlen      = withKnownNat n $ natVal n
      predicate = fromIntegral nlen < num
      errMsg    = path <> " should be < " <> T.pack (show nlen)
      warn      = vWarning $ mmSingleton path (pure errMsg)
    unless predicate warn
  SNLe n -> do
    let
      nlen      = withKnownNat n $ natVal n
      predicate = fromIntegral nlen <= num
      errMsg    = path <> " should be <= " <> T.pack (show nlen)
      warn      = vWarning $ mmSingleton path (pure errMsg)
    unless predicate warn

validateArrayConstraint
  :: JSONPath
  -> V.Vector a
  -> Sing (tcs :: ArrayConstraint)
  -> Validation ()
validateArrayConstraint (JSONPath path) v = \case
  SAEq n -> do
    let
      nlen      = withKnownNat n $ natVal n
      predicate = nlen == fromIntegral (V.length v)
      errMsg    = "length of " <> path <> " should be == " <> T.pack (show nlen)
      warn      = vWarning $ mmSingleton path (pure errMsg)
    unless predicate warn

validateJsonRepr
  :: Sing schema
  -> [DemotedPathSegment]
  -> JsonRepr schema
  -> Validation ()
validateJsonRepr sschema dpath jr = case jr of
  ReprText t -> case sschema of
    SSchemaText scs -> do
      let
        process :: Sing (cs :: [TextConstraint]) -> Validation ()
        process SNil         = pure ()
        process (SCons c cs) = do
          validateTextConstraint (demotedPathToText dpath) t c
          process cs
      process scs
  ReprNumber n -> case sschema of
    SSchemaNumber scs -> do
      let
        process :: Sing (cs :: [NumberConstraint]) -> Validation ()
        process SNil         = pure ()
        process (SCons c cs) = do
          validateNumberConstraint (demotedPathToText dpath) n c
          process cs
      process scs
  ReprNull -> pure ()
  ReprBoolean _ -> pure ()
  ReprArray v -> case sschema of
    SSchemaArray acs s -> do
      let
        process :: Sing (cs :: [ArrayConstraint]) -> Validation ()
        process SNil         = pure ()
        process (SCons c cs) = do
          validateArrayConstraint (demotedPathToText dpath) v c
          process cs
      process acs
      for_ (V.indexed v) $ \(ix, jr') -> do
        let newPath = dpath <> pure (DIx $ fromIntegral ix)
        validateJsonRepr s newPath jr'
  ReprOptional d -> case sschema of
    SSchemaOptional ss -> case d of
      Just x  -> validateJsonRepr ss dpath x
      Nothing -> pure ()
  ReprObject fs -> case sschema of
    SSchemaObject _ -> go fs
      where
        go :: Rec FieldRepr (ts :: [(Symbol, Schema)] ) -> Validation ()
        go RNil                     = pure ()
        go (f@(FieldRepr d) :& ftl) = do
          let newPath = dpath <> [DKey (knownFieldName f)]
          validateJsonRepr (knownFieldSchema f) newPath d
          go ftl
  ReprUnion _ -> pure () -- FIXME
    -- case sschema of
    --   SSchemaUnion ss -> case ss of
    --     SCons s stl -> case umatch' s u of
    --       Nothing -> case urestrict u of
    --         Nothing ->
    --           fail "impossible to produce subUnion, please report this as a bug"
    --         Just x -> do
    --           let
    --             JSONPath path = demotedPathToText dpath
    --           case stl of
    --             SNil -> void $ vWarning $ mmSingleton path
    --               $ pure "union handling error, please report this as bug"
    --             SCons s' stl' ->
    --               validateJsonRepr (SSchemaUnion (SCons s' stl')) dpath
    --                 $ toUnion (SCons s' stl') x
    --       Just x  -> validateJsonRepr s dpath x

-- subUnion
--   :: Sing (s ': stl)
--   -> (  USubset stl (s ': stl) (RImage stl (s ': stl))
--      => Union f (s ': stl)
--      -> Maybe (Union f stl) )
-- subUnion (SCons s stl) = urestrict

-- withUSubset
--   :: Sing (s ': stl)
--   -> (USubset stl (s ': stl) (RImage stl (s ': stl)) => Maybe (Union f stl))
--   -> Maybe (Union f stl)
-- withUSubset (SCons s stl) r = r

toUnion
  :: USubset s' (s ': ss) (RImage s' (s ': ss))
  => Sing (s ': ss)
  -> Union JsonRepr s'
  -> JsonRepr ('SchemaUnion (s ': ss))
toUnion _ = ReprUnion . urelax

umatch' :: UElem a as i => Sing a -> Union f as -> Maybe (f a)
umatch' _ u = umatch u

parseAndValidateJson
  :: forall schema
  .  (FromJSON (JsonRepr schema), TopLevel schema, SingI schema)
  => Value
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
  :: (FromJSON (JsonRepr schema), TopLevel schema, SingI schema)
  => proxy schema
  -> Value
  -> ParseResult (JsonRepr schema)
parseAndValidateJsonBy _ = parseAndValidateJson
