{-# OPTIONS -fprint-explicit-kinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Schematic
  ( module Data.Schematic.Schema
  , module Data.Schematic.Utils
  , decodeAndValidateJson
  , parseAndValidateJson
  , parseAndValidateVersionedJson
  , isValid
  , isDecodingError
  , isValidationError
  , ParseResult(..)
  ) where

import Control.Monad.Validation
import Data.Aeson as J
import Data.Aeson.Types as J
import Data.ByteString.Lazy as BL
import Data.Functor.Identity
import Data.Schematic.Migration
import Data.Schematic.Schema
import Data.Schematic.Utils
import Data.Schematic.Validation
import Data.Singletons.Prelude
import Data.Text as T


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

parseAndValidateJsonBySing
  :: forall schema proxy
  . (J.FromJSON (JsonRepr schema), TopLevel schema)
  => Sing schema
  -> J.Value
  -> ParseResult (JsonRepr schema)
parseAndValidateJsonBySing sschema v = case parseEither parseJSON v of
  Left s         -> DecodingError $ T.pack s
  Right jsonRepr ->
    let
      validate = validateJsonRepr sschema [] jsonRepr
      res      = runIdentity . runValidationTEither $ validate
    in case res of
      Left em  -> ValidationError em
      Right () -> Valid jsonRepr

-- parseAndValidateVersionedJson
--   :: MList (m ': ms)
--   -> J.Value
--   -> ParseResult (JsonRepr)

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

-- [("top version", schemaTopVersion), ("n-1 version", schemaNminusOneVersion), ...]

-- type family Migratable (rs :: [(Revision, Schema)]) :: Constraint where
--   -- constraint duplication
--   Migratable ('(r,s) ': '(r', s') ': tl) =
--     (SingI s, MigrateSchema s s', TopLevel s, Migratable ('(r',s') ': tl))
--   Migratable ('(r,s) ': '[])             = (TopLevel s, SingI s)
--   -- Migratable '[]                         = ('True ~ 'False)

class Migratable (revisions :: [(Revision, Schema)]) where
  mparse :: Sing revisions -> J.Value -> Either String (JsonRepr (Snd (Head revisions)))

instance {-# OVERLAPPING #-} (Migratable (Tail revisions), MigrateSchema (Snd (Head (Tail revisions))) (Snd (Head revisions)), SingI (Snd (Head revisions))) => Migratable revisions where
  mparse s v = case parseEither parseJSON v of
    Left _  -> migrate <$> mparse (sTail s) v
    Right x -> Right x

instance {-# OVERLAPPABLE #-} (TopLevel (Snd rev), SingI (Snd rev)) => Migratable '[rev] where
  mparse _ = parseEither parseJSON

parseAndValidateVersionedJson
  :: forall proxy v. (SingI (AllVersions v), Migratable (AllVersions v))
  => proxy v
  -> J.Value
  -> Either String (JsonRepr (Snd (Head (AllVersions v))))
parseAndValidateVersionedJson s v = mparse (sing :: Sing (AllVersions v)) v

decodeAndValidateJson
  :: forall schema
  .  (J.FromJSON (JsonRepr schema), TopLevel schema, SingI schema)
  => BL.ByteString
  -> ParseResult (JsonRepr schema)
decodeAndValidateJson bs = case decode bs of
  Nothing -> DecodingError "invalid json"
  Just x  -> parseAndValidateJson x
