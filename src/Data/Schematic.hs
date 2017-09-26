{-# OPTIONS -fprint-explicit-kinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Schematic
  ( module Data.Schematic.JsonSchema
  , module Data.Schematic.Schema
  , module Data.Schematic.Lens
  , module Data.Schematic.Migration
  , module Data.Schematic.Utils
  , decodeAndValidateJson
  , parseAndValidateJson
  , parseAndValidateJsonBy
  , parseAndValidateTopVersionJson
  , parseAndValidateWithMList
  , decodeAndValidateVersionedWithMList
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
import Data.Schematic.JsonSchema
import Data.Schematic.Lens
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
