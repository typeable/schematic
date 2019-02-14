module Data.Schematic.Generator where

import Data.Aeson (Value(..))
import qualified Data.HashMap.Lazy as H
import Data.Maybe
import Data.Schematic.Schema
import Data.Text (Text, pack)
import qualified Data.Vector as V
import Data.Schematic.Generator.Regex
import Data.Schematic.Verifier
import Test.SmallCheck.Series

maxHigh :: Int
maxHigh = 30

minLow :: Int
minLow = 2

textLengthSeries :: Monad m => [VerifiedTextConstraint] -> Series m Text
textLengthSeries =
  \case
    [VTEq eq] -> pure $ pack $ take (fromIntegral eq) $ cycle "sample"
    [VTBounds ml mh] -> do
      let l = fromMaybe minLow (fromIntegral <$> ml) + 1
          h = fromMaybe maxHigh (fromIntegral <$> mh) - 1
      n <- generate $ \depth -> take depth [l .. h]
      pure $ pack $ take (fromIntegral n) $ cycle "sample"
    _ -> pure "error"

textEnumSeries :: Monad m => [Text] -> Series m Text
textEnumSeries enum = generate $ \depth -> take depth enum

textSeries :: Monad m => [DemotedTextConstraint] -> Series m Value
textSeries cs = do
  let mvcs = verifyTextConstraints cs
  case mvcs of
    Just vcs -> do
      n <- textSeries' vcs
      pure $ String n
    Nothing -> pure Null

textSeries' :: Monad m => [VerifiedTextConstraint] -> Series m Text
textSeries' [] = pure "sample"
textSeries' vcs = do
  let enums = listToMaybe [x | VTEnum x <- vcs]
  case enums of
    Just e -> textEnumSeries e
    Nothing -> do
      let regexps = listToMaybe [x | VTRegex x _ _ <- vcs]
      case regexps of
        Just e -> regexSeries e
        Nothing -> textLengthSeries vcs

numberSeries :: Monad m => [DemotedNumberConstraint] -> Series m Value
numberSeries cs = do
  let mvcs = verifyNumberConstraints cs
  case mvcs of
    Just vcs -> do
      n <- numberSeries' vcs
      pure $ Number $ fromIntegral n
    Nothing -> pure Null

numberSeries' :: Monad m => VerifiedNumberConstraint -> Series m Integer
numberSeries' =
  \case
    VNEq eq -> pure $ fromIntegral eq
    VNBounds ml mh -> do
      let l = fromMaybe minLow (fromIntegral <$> ml) + 1
          h = fromMaybe maxHigh (fromIntegral <$> mh) - 1
      n <- generate $ \depth -> take depth [l .. h]
      pure $ fromIntegral n

arraySeries ::
     Monad m => [DemotedArrayConstraint] -> DemotedSchema -> Series m Value
arraySeries cs sch = do
  let mvcs = verifyArrayConstraint cs
  case mvcs of
    Just vcs -> arraySeries' vcs sch
    Nothing -> pure Null

arraySeries' ::
     Monad m => Maybe VerifiedArrayConstraint -> DemotedSchema -> Series m Value
arraySeries' ml sch = do
  objs <- V.replicateM (maybe minRepeat f ml) (valueSeries sch)
  pure $ Array objs
  where
    f (VAEq l) = fromIntegral l

valueSeries :: Monad m => DemotedSchema -> Series m Value
valueSeries (DSchemaText cs) = textSeries cs
valueSeries (DSchemaNumber cs) = numberSeries cs
valueSeries DSchemaBoolean = Bool <$> pure True \/ pure False
valueSeries (DSchemaObject pairs') =
  Object <$> (decDepth $ mapM valueSeries $ H.fromList pairs')
valueSeries (DSchemaArray cs sch) = arraySeries cs sch
valueSeries DSchemaNull = pure Null
valueSeries (DSchemaOptional sch) = pure Null \/ valueSeries sch
valueSeries (DSchemaUnion schs) = do
  objs <- mapM valueSeries schs
  pure $ Array $ V.fromList objs
