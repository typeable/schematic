{-# OPTIONS -fprint-explicit-kinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Schematic
  ( module Data.Schematic.Schema
  , module Data.Schematic.Migration
  , module Data.Schematic.Utils
  , decodeAndValidateJson
  , parseAndValidateJson
  , parseAndValidateVersionedJson
  , parseAndValidateTopVersionJson
  , decodeAndValidateVersionedJson
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

instance {-# OVERLAPPING #-} (TopLevel (Snd rev), SingI (Snd rev)) => Migratable '[rev] where
  mparse _ = parseAndValidateJson

instance {-# OVERLAPPABLE #-}
  ( Migratable (Tail revisions)
  , MigrateSchema (Snd (Head (Tail revisions))) (Snd (Head revisions))
  , SingI (Snd (Head revisions)))
  => Migratable revisions where
  mparse s v = case parseEither parseJSON v of
    Left _  -> migrate <$> mparse (sTail s) v
    Right x -> Valid x
-- type family Migratable (revisions :: [(Revision, Schema)]) = (cs :: Constraint) where
--   Migratable '[rev] = (TopLevel (Snd rev), SingI (Snd rev))
--   Migratable  revisions =
--    ( Migratable (Tail revisions)
--    , MigrateSchema (Snd (Head (Tail revisions))) (Snd (Head revisions))
--    , SingI (Snd (Head revisions)))

-- mparseBySing
--   :: Sing schema
--   -> J.Value
--   -> ParseResult (JsonRepr (Snd (Head revisions)))
-- mparseBySing s v = undefined

-- mparse
--   :: Migratable revisions
--   => Sing revisions
--   -> J.Value
--   -> ParseResult (JsonRepr (Snd (Head revisions)))
-- mparse s v =
--   let
--     st = sTail s
--     sth = sSnd $ sHead st
--   SCons x SNil -> parseAndValidateJson v
--   SCons sch _   -> case parseEither parseJSON v of
--     Left _  -> migrate <$> mparse (sTail s) v
--     Right x -> Valid x

parseAndValidateVersionedJson
  :: forall proxy v. (SingI (AllVersions v), Migratable (AllVersions v))
  => proxy v
  -> J.Value
  -> ParseResult (JsonRepr (Snd (Head (AllVersions v))))
parseAndValidateVersionedJson _ v = mparse (sing :: Sing (AllVersions v)) v

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
