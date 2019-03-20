module Data.Schematic.Generator where

import Control.Applicative
import Data.Maybe
import Data.Schematic.Constraints
import Data.Schematic.Generator.Regex
import Data.Schematic.Verifier
import Data.Scientific
import Data.Text (Text, pack)
-- import qualified Data.Vector as V
import Test.SmallCheck.Series


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

textSeries :: Monad m => [TextConstraintT] -> Series m Text
textSeries cs = maybe (pure "error") textSeries' $ verifyTextConstraints cs

textSeries' :: Monad m => [VerifiedTextConstraint] -> Series m Text
textSeries' [] = pure "sample"
textSeries' vcs
  = fromMaybe (textLengthSeries vcs)
  $ textEnumSeries <$> listToMaybe [x | VTEnum x <- vcs]
  <|> regexSeries <$> listToMaybe [x | VTRegex x _ _ <- vcs]

numberSeries :: Monad m => [NumberConstraintT] -> Series m Scientific
numberSeries cs = maybe (pure 0) numberSeries' $ verifyNumberConstraints cs

numberSeries' :: Monad m => VerifiedNumberConstraint -> Series m Scientific
numberSeries' =
  \case
    VNEq eq -> pure $ fromIntegral eq
    VNBounds ml mh -> do
      let l = fromMaybe minLow (fromIntegral <$> ml) + 1
          h = fromMaybe maxHigh (fromIntegral <$> mh) - 1
      n <- generate $ \depth -> take depth [l .. h]
      pure $ fromIntegral n

-- arraySeries
--   :: (Monad m, Serial m (JsonRepr s))
--   => [ArrayConstraintT] -> Series m (V.Vector (JsonRepr s))
-- arraySeries cs = maybe (pure V.empty) arraySeries' $ verifyArrayConstraint cs
--
-- arraySeries'
--   :: forall m s. (Monad m, Serial m (JsonRepr s))
--   => Maybe VerifiedArrayConstraint -> Series m (V.Vector (JsonRepr s))
-- arraySeries' ml =
--   V.replicateM (maybe minRepeat f ml) (series :: Series m (JsonRepr s))
--   where
--     f (VAEq l) = fromIntegral l
