{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}

module Data.Schematic.JsonSchema
  ( toJsonSchema
  , toJsonSchema'
  ) where

import Control.Monad.State.Strict
import Data.Aeson as J
import Data.Foldable as F
import Data.HashMap.Strict as H
import Data.List as L
import Data.List.NonEmpty as NE
import Data.Schematic.Constraints
import Data.Schematic.Schema as S
import Data.Set as Set
import Data.Singletons
import Data.Text
import Data.Traversable
import JSONSchema.Draft4.Schema as D4
import JSONSchema.Validator.Draft4 as D4


draft4 :: Text
draft4 = "http://json-schema.org/draft-04/schema#"

textConstraint :: TextConstraintT -> State D4.Schema ()
textConstraint (TEq n) = modify $ \s -> s
  { _schemaMinLength = pure $ fromIntegral n
  , _schemaMaxLength = pure $ fromIntegral n }
textConstraint (TLt n) = modify $ \s -> s
  { _schemaMaxLength = pure . fromIntegral $ n + 1 }
textConstraint (TLe n) = modify $ \s -> s
  { _schemaMaxLength = pure . fromIntegral $ n }
textConstraint (TGt n) =
  let n' = if n == 0 then 0 else n - 1
  in modify $ \s -> s { _schemaMinLength = pure . fromIntegral $ n' }
textConstraint (TGe n) = modify $ \s -> s
  { _schemaMinLength = pure . fromIntegral $ n }
textConstraint (TRegex r) = modify $ \s -> s { _schemaPattern = pure r }
textConstraint (TEnum ss) =
  let ss' = if F.length ss == 0 then [] else NE.fromList $ J.String <$> ss
  in modify $ \s -> s { _schemaEnum = pure ss' }

numberConstraint :: NumberConstraintT -> State D4.Schema ()
numberConstraint (NLe n) = modify $ \s -> s
  { _schemaMaximum = pure . fromIntegral $ n }
numberConstraint (NLt n) = modify $ \s -> s
  { _schemaMaximum = pure . fromIntegral $ n + 1 }
numberConstraint (NGt n) = modify $ \s -> s
  { _schemaMinimum = pure . fromIntegral $ n }
numberConstraint (NGe n) =
  let n' = if n == 0 then 0 else n - 1
  in modify $ \s -> s { _schemaMinimum = pure . fromIntegral $ n' }
numberConstraint (NEq n) = modify $ \s -> s
  { _schemaMinimum = pure $ fromIntegral n
  , _schemaMaximum = pure $ fromIntegral n }

arrayConstraint :: ArrayConstraintT -> State D4.Schema ()
arrayConstraint (AEq _) = pure ()

toJsonSchema
  :: forall proxy schema
   . SingI schema
  => proxy (schema :: S.Schema)
  -> Maybe D4.Schema
toJsonSchema _ = do
  js <- toJsonSchema' $ fromSing (sing :: Sing schema)
  pure $ js { _schemaVersion = pure draft4 }

toJsonSchema'
  :: SchemaT
  -> Maybe D4.Schema
toJsonSchema' = \case
  SchemaText tcs ->
    pure $ execState (traverse_ textConstraint tcs) $ emptySchema
      { _schemaType = pure $ TypeValidatorString D4.SchemaString }
  S.SchemaNumber ncs ->
    pure $ execState (traverse_ numberConstraint ncs) $ emptySchema
      { _schemaType = pure $ TypeValidatorString D4.SchemaNumber }
  S.SchemaBoolean -> pure $ emptySchema
    { _schemaType = pure $ TypeValidatorString D4.SchemaBoolean }
  S.SchemaObject objs -> do
    res <- for objs $ \(n,s) -> do
      s' <- toJsonSchema' s
      pure (n, s')
    let
      nonOpt = \case
        (_, SchemaOptional _) -> False
        _                     -> True
    pure $ emptySchema
      { _schemaType       = pure $ TypeValidatorString D4.SchemaObject
      , _schemaRequired   = pure $ Set.fromList $ fst <$> L.filter nonOpt objs
      , _schemaProperties = pure $ H.fromList res }
  S.SchemaArray acs sch -> do
    res <- toJsonSchema' sch
    pure $ execState (traverse_ arrayConstraint acs) $ emptySchema
      { _schemaType  = pure $ TypeValidatorString D4.SchemaArray
      , _schemaItems = pure $ ItemsObject res }
  S.SchemaNull -> pure $ emptySchema
    { _schemaType = pure $ TypeValidatorString D4.SchemaNull }
  SchemaOptional sch -> do
    snull <- toJsonSchema' S.SchemaNull
    sres <- toJsonSchema' sch
    pure $ emptySchema { _schemaOneOf = pure (snull :| [sres]) }
  SchemaUnion sch -> do
    schemaUnion <- traverse toJsonSchema' sch >>= \case
      [] -> Nothing
      x  -> Just x
    pure $ emptySchema { _schemaAnyOf = pure $ NE.fromList schemaUnion }
