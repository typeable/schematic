{-# OPTIONS -fprint-explicit-kinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Schematic
  ( module Data.Schematic.JsonSchema
  , module Data.Schematic.Helpers
  , module Data.Schematic.Lens
  , module Data.Schematic.Migration
  , module Data.Schematic.Schema
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
  , bigSchema
  ) where

import Control.Monad.Validation
import Data.Aeson as J
import Data.Aeson.Types as J
import Data.ByteString.Lazy as BL
import Data.Functor.Identity as F
import Data.Schematic.DSL
import Data.Schematic.Helpers
import Data.Schematic.JsonSchema
import Data.Schematic.Lens
import Data.Schematic.Migration
import Data.Schematic.Schema
import Data.Schematic.Validation
import Data.Singletons.Prelude hiding ((:.))
import Data.Tagged
import Data.Text as T
import Data.Vinyl


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
parseAndValidateWithMList (Tagged f :&& tl) v =
  case parseAndValidateJsonBy Proxy v of
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

type SText = SchemaText '[]
-- type SText = SchemaText '[TRegex "[0-9]+\\.[0-9]{2}"]
-- type SEnum = SchemaText '[TEnum '["one", "two", "three"]]

type BigSchema = SchemaObject
  '[ '("1", SText)
   , '("2", SText)
   , '("3", SText)
   , '("4", SText)
   , '("5", SText)
   , '("6", SText)
   , '("7", SText)
   , '("8", SText)
   , '("9", SText)
   , '("10", SText)
   , '("11", SText)
   , '("12", SText)
   , '("13", SText)
   , '("14", SText)
   , '("15", SText)
   , '("16", SText)
   , '("17", SText)
   , '("18", SText)
   , '("19", SText)
   , '("20", SText)
   , '("21", SText)
   , '("22", SText)
   , '("23", SText)
   , '("24", SText)
   , '("25", SText)
   , '("26", SText)
   , '("27", SText)
   , '("28", SText)
   , '("29", SText)
   , '("30", SText)
   ]

bigSchema :: JsonRepr BigSchema
bigSchema = withRepr @BigSchema
  $  field @"1" ""
  :& field @"2" ""
  :& field @"3" ""
  :& field @"4" ""
  :& field @"5" ""
  :& field @"6" ""
  :& field @"7" ""
  :& field @"8" ""
  :& field @"9" ""
  :& field @"10" ""
  :& field @"11" ""
  :& field @"12" ""
  :& field @"13" ""
  :& field @"14" ""
  :& field @"15" ""
  :& field @"16" ""
  :& field @"17" ""
  :& field @"18" ""
  :& field @"19" ""
  :& field @"20" ""
  :& field @"21" ""
  :& field @"22" ""
  :& field @"23" ""
  :& field @"24" ""
  :& field @"25" ""
  :& field @"26" ""
  :& field @"27" ""
  :& field @"28" ""
  :& field @"29" ""
  :& field @"30" ""
  :& RNil
