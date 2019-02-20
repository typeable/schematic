module Data.Schematic.Generator where

import           Data.Maybe
import           Data.Schematic.Generator.Regex
import {-# SOURCE #-} Data.Schematic.Schema
import           Data.Schematic.Verifier
import           Data.Scientific
import           Data.Text (Text, pack)
import qualified Data.Vector as V
import           Test.SmallCheck.Series

maxHigh :: Int
maxHigh = 30

minLow :: Int
minLow = 2

textLengthSeries :: Monad m => [VerifiedTextConstraint] -> Series m Text
textLengthSeries =
  \case
    [VTEq eq]        -> pure $ pack $ take (fromIntegral eq) $ cycle "sample"
    [VTBounds ml mh] -> do
      let
        l = fromMaybe minLow (fromIntegral <$> ml) + 1
        h = fromMaybe maxHigh (fromIntegral <$> mh) - 1
      n <- generate $ \depth -> take depth [l .. h]
      pure $ pack $ take (fromIntegral n) $ cycle "sample"
    _                -> pure "error"

textEnumSeries :: Monad m => [Text] -> Series m Text
textEnumSeries enum = generate $ \depth -> take depth enum

textSeries :: Monad m => [DemotedTextConstraint] -> Series m Text
textSeries cs = do
  let mvcs = verifyTextConstraints cs
  case mvcs of
    Just vcs -> do
      n <- textSeries' vcs
      pure n
    Nothing  -> pure "error"

textSeries' :: Monad m => [VerifiedTextConstraint] -> Series m Text
textSeries' [] = pure "sample"
textSeries' vcs = do
  let enums = listToMaybe [x | VTEnum x <- vcs]
  case enums of
    Just e -> textEnumSeries e
    Nothing -> do
      let regexps = listToMaybe [x | VTRegex x _ _ <- vcs]
      case regexps of
        Just e  -> regexSeries e
        Nothing -> textLengthSeries vcs

numberSeries :: Monad m => [DemotedNumberConstraint] -> Series m Scientific
numberSeries cs = do
  let mvcs = verifyNumberConstraints cs
  case mvcs of
    Just vcs -> do
      n <- numberSeries' vcs
      pure $ n
    Nothing -> pure 0

numberSeries' :: Monad m => VerifiedNumberConstraint -> Series m Scientific
numberSeries' =
  \case
    VNEq eq -> pure $ fromIntegral eq
    VNBounds ml mh -> do
      let l = fromMaybe minLow (fromIntegral <$> ml) + 1
          h = fromMaybe maxHigh (fromIntegral <$> mh) - 1
      n <- generate $ \depth -> take depth [l .. h]
      pure $ fromIntegral n

arraySeries
  :: (Monad m, Serial m (JsonRepr s))
  => [DemotedArrayConstraint]
  -> Series m (V.Vector (JsonRepr s))
arraySeries cs = do
  let mvcs = verifyArrayConstraint cs
  case mvcs of
    Just vcs -> arraySeries' vcs
    Nothing  -> pure V.empty

arraySeries'
  :: forall m s. (Monad m, Serial m (JsonRepr s))
  => Maybe VerifiedArrayConstraint
  -> Series m (V.Vector (JsonRepr s))
arraySeries' ml = do
  objs <- V.replicateM (maybe minRepeat f ml) (series :: Series m (JsonRepr s))
  pure $ objs
  where
    f (VAEq l) = fromIntegral l
