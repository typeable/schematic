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
  .  (J.FromJSON (JsonRepr schema), TopLevel schema, Known (Sing schema))
  => J.Value
  -> ParseResult (JsonRepr schema)
parseAndValidateJson v =
  case parseEither parseJSON v of
    Left s         -> DecodingError $ T.pack s
    Right jsonRepr ->
      let
        validate = validateJsonRepr (known :: Sing schema) [] jsonRepr
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

parseAndValidateTopVersionJson
  :: forall proxy (v :: Versioned)
  .  (Known (Sing (TopVersion (AllVersions v))))
  => proxy v
  -> J.Value
  -> ParseResult (JsonRepr (TopVersion (AllVersions v)))
parseAndValidateTopVersionJson _ v =
  case parseEither parseJSON v of
    Left s -> DecodingError $ T.pack s
    Right jsonRepr ->
      let
        validate =
          validateJsonRepr (known :: Sing (TopVersion (AllVersions v))) [] jsonRepr
        res      = runIdentity . runValidationTEither $ validate
      in case res of
        Left em  -> ValidationError em
        Right () -> Valid jsonRepr

parseAndValidateVersionedJson
  :: forall proxy rav av (v :: Versioned) rs tv
  . ( Known (Sing (AllVersions v))
    , Migratable (SchemaPairs (AllVersions v))
    , (Known (Sing (TopVersion (AllVersions v))))
    , (Known (Sing (SchemaPairs (AllVersions v)))))
  => proxy v
  -> J.Value
  -> ParseResult (JsonRepr (TopVersion (AllVersions v)))
parseAndValidateVersionedJson _ v =
  let
    rss :: Sing (SchemaPairs (AllVersions v))
    rss = known
    stv :: Sing (TopVersion (AllVersions v))
    stv = known
  in parseAndValidateVersionedJsonByVersions stv rss v

parseAndValidateVersionedJsonByVersions
  :: forall (rs :: [(Revision, Schema)]) tv proxy
  . (Migratable rs)
  => proxy (tv :: Schema)
  -> Sing rs
  -> J.Value
  -> ParseResult (JsonRepr tv)
parseAndValidateVersionedJsonByVersions stv srs value = case srs of
  SNil -> DecodingError "blabla"
  SCons (STuple2 r s) tl -> case parseAndValidateJsonBySing s value of
    Valid a -> Valid $ migrate a
    _       -> DecodingError "dummy plug"

decodeAndValidateJson
  :: forall schema
  .  (J.FromJSON (JsonRepr schema), TopLevel schema, Known (Sing schema))
  => BL.ByteString
  -> ParseResult (JsonRepr schema)
decodeAndValidateJson bs = case decode bs of
  Nothing -> DecodingError "invalid json"
  Just x  -> parseAndValidateJson x
