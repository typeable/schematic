{-# LANGUAGE CPP #-}
module Data.Schematic.Validation where

import Control.Monad
import Control.Monad.Validation
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import Data.Functor.Identity
import Data.Schematic.Constraints
import Data.Schematic.Path
import Data.Schematic.Schema
import Data.Scientific
import Data.Singletons.Prelude
import Data.Text as T
import Data.Traversable
import Data.Union
import Data.Vector as V
import Data.Vinyl
import Data.Vinyl.TypeLevel
import Prelude as P
import Text.Regex.TDFA
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif


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
isValid _         = False

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
validateTextConstraint (JSONPath path) t s =
  case (fromSing s :: TextConstraintT) of
    TEq n -> checkLength n (==) "=="
    TLt n -> checkLength n (<) "<"
    TLe n -> checkLength n (<=) "<="
    TGt n -> checkLength n (>) ">"
    TGe n -> checkLength n (>=) ">="
    TRegex r -> unless
      (matchTest (makeRegex (T.unpack r) :: Regex) $ T.unpack t)
      $ vWarning $ mmSingleton path $ pure $ path <> " must match " <> r
    TEnum ss -> unless (t `P.elem` ss) $ vWarning $ mmSingleton path
      $ pure $ path <> " must be one of " <> T.pack (show ss)
  where
    checkLength n f sf =
      unless (f (fromIntegral $ T.length t) n)
        $ vWarning $ mmSingleton path $ pure
          $ "length of " <> path <> " should be " <> sf
            <> " " <> T.pack (show n)

validateNumberConstraint
  :: JSONPath
  -> Scientific
  -> Sing (tcs :: NumberConstraint)
  -> Validation ()
validateNumberConstraint (JSONPath path) num s =
  case (fromSing s :: NumberConstraintT) of
    NEq n -> checkVal n (==) "=="
    NLt n -> checkVal n (<) "<"
    NLe n -> checkVal n (<=) "<="
    NGt n -> checkVal n (>) ">"
    NGe n -> checkVal n (>=) ">="
  where
    checkVal n f sf =
      unless (f num $ fromIntegral n)
        $ vWarning $ mmSingleton path $ pure
          $ path <> " should be " <> sf <> " " <> T.pack (show n)

validateArrayConstraint
  :: JSONPath
  -> V.Vector a
  -> Sing (tcs :: ArrayConstraint)
  -> Validation ()
validateArrayConstraint (JSONPath path) v s =
  case (fromSing s :: ArrayConstraintT) of
   AEq n -> unless (V.length v == fromIntegral n)
    $ vWarning $ mmSingleton path $ pure
      $ "length of " <> path <> " should be == " <> T.pack (show n)

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
        let newPath = dpath <> pure (Ix $ fromIntegral ix)
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
          let newPath = dpath <> [Key (knownFieldName f)]
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
  :: (USubset s' (s ': ss) (RImage s' (s ': ss)), ReprUnionConstr ss)
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

-- parseAndValidateJsonBy
--   :: (FromJSON (JsonRepr schema), TopLevel schema, SingI schema)
--   => proxy schema
--   -> Value
--   -> ParseResult (JsonRepr schema)
-- parseAndValidateJsonBy _ = parseAndValidateJson
