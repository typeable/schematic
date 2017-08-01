{-# OPTIONS -fprint-explicit-kinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Schematic
  ( module Data.Schematic.Schema
  , module Data.Schematic.Migration
  , module Data.Schematic.Utils
  , decodeAndValidateJson
  , parseAndValidateJson
  , parseAndValidateJsonBy
  , parseAndValidateVersionedJson
  , parseAndValidateTopVersionJson
  , decodeAndValidateVersionedJson
  , parseAndValidateWithMList
  , decodeAndValidateVersionedWithMList
  , isValid
  , isDecodingError
  , isValidationError
  , ParseResult(..)
  , Migratable
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

class Migratable (revisions :: [(Revision, Schema)]) where
  mparse
    :: Sing revisions
    -> J.Value
    -> ParseResult (JsonRepr (Snd (Head revisions)))

instance
  {-# OVERLAPPING #-}
  ( TopLevel (Snd rev), SingI (Snd rev) )
  => Migratable '[rev] where
  mparse _ = parseAndValidateJson

instance {-# OVERLAPPABLE #-}
  ( Migratable (Tail revisions)
  , MigrateSchema (Snd (Head (Tail revisions))) (Snd (Head revisions))
  , SingI (Snd (Head revisions)))
  => Migratable revisions where
  mparse s v = case parseEither parseJSON v of
    Left _  ->
      migrate <$> (mparse (sTail s) v :: ParseResult (JsonRepr (Snd (Head (Tail revisions)))))
    Right x -> Valid x

parseAndValidateVersionedJson
  :: forall proxy v. (SingI (AllVersions v), Migratable (AllVersions v))
  => proxy v
  -> J.Value
  -> ParseResult (JsonRepr (Snd (Head (AllVersions v))))
parseAndValidateVersionedJson _ v = mparse (sing :: Sing (AllVersions v)) v

parseAndValidateWithMList
  :: MList revisions
  -> J.Value
  -> ParseResult (JsonRepr (Head revisions))
parseAndValidateWithMList MNil v = parseAndValidateJson v
parseAndValidateWithMList ((:&&) p f tl) v = case parseAndValidateJsonBy p v of
    Valid a           -> Valid a
    DecodingError _   -> f <$> parseAndValidateWithMList tl v
    ValidationError _ -> f <$> parseAndValidateWithMList tl v

decodeAndValidateJson
  :: forall schema
  .  (J.FromJSON (JsonRepr schema), TopLevel schema, SingI schema)
  => BL.ByteString
  -> ParseResult (JsonRepr schema)
decodeAndValidateJson bs = case decode bs of
  Nothing -> DecodingError "malformed json"
  Just x  -> parseAndValidateJson x

decodeAndValidateVersionedJson
  :: (Migratable (AllVersions versioned), SingI (AllVersions versioned))
  => proxy versioned
  -> BL.ByteString
  -> ParseResult (JsonRepr (Snd (Head (AllVersions versioned))))
decodeAndValidateVersionedJson vp bs = case decode bs of
  Nothing -> DecodingError "malformed json"
  Just x  -> parseAndValidateVersionedJson vp x

type family MapSnd (l :: [(a,k)]) = (r :: [k]) where
  MapSnd '[] = '[]
  MapSnd ( '(a, b) ': tl) = b ': MapSnd tl

decodeAndValidateVersionedWithMList
  :: proxy versioned
  -> MList (MapSnd (AllVersions versioned))
  -> BL.ByteString
  -> ParseResult (JsonRepr (Head (MapSnd (AllVersions versioned))))
decodeAndValidateVersionedWithMList _ mlist bs = case decode bs of
  Nothing -> DecodingError "malformed json"
  Just x  -> parseAndValidateWithMList mlist x
