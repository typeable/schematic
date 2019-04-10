module Data.Schematic.Validation where

import Control.Monad
import Control.Monad.Validation
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import Data.Functor.Identity
import Data.Monoid ((<>))
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

class ValidateConstraint t c where
  validateConstraint
    :: [DemotedPathSegment] -> t -> Sing (a::c) -> Validation ()

instance ValidateConstraint Text TextConstraint where
  validateConstraint = validateTextConstraint . demotedPathToText

instance ValidateConstraint Scientific NumberConstraint where
  validateConstraint = validateNumberConstraint . demotedPathToText

instance ValidateConstraint (V.Vector a) ArrayConstraint where
  validateConstraint = validateArrayConstraint . demotedPathToText

validateConstraints
  :: ValidateConstraint t c
  => [DemotedPathSegment] -> t -> Sing (cs :: [c]) -> Validation ()
validateConstraints _ _ SNil = pure ()
validateConstraints dp t (SCons c cs) = do
  validateConstraint dp t c >> validateConstraints dp t cs

validateJsonRepr
  :: Sing schema
  -> [DemotedPathSegment]
  -> JsonRepr schema
  -> Validation ()
validateJsonRepr sschema dpath jr = case jr of
  ReprText t -> case sschema of
    SSchemaText scs -> validateConstraints dpath t scs
  ReprNumber n -> case sschema of
    SSchemaNumber scs -> validateConstraints dpath n scs
  ReprNull -> pure ()
  ReprBoolean _ -> pure ()
  ReprArray v -> case sschema of
    SSchemaArray acs s -> do
      validateConstraints dpath v acs
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
  ReprUnion ru -> -- pure () -- FIXME
    case sschema of
      SSchemaUnion su -> validateUnion su ru
    where
      validateUnion
        :: forall (us :: [Schema])
        . Sing us -> Union JsonRepr us -> Validation ()
      validateUnion ss r  = case (ss,r) of
        (SCons (s :: Sing su) _, This v) -> validateJsonRepr s dpath v
        (SCons _ stl, That r')           -> validateUnion stl r'
        (SNil,_) -> fail "Invalid union. Please report this as a bug"

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
