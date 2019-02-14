module Data.Schematic.Generator.Regex where

import Control.Monad
import Data.Aeson (Object, Value(..))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Schematic.Schema
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, singleton, toLazyText)
import qualified Data.Vector as V
import Test.SmallCheck
import Test.SmallCheck.Series
import Text.Regex.TDFA
import Text.Regex.TDFA.Pattern
import Text.Regex.TDFA.ReadRegex (parseRegex)
import Data.Schematic.Verifier


minRepeat :: Int
minRepeat = 2

maxRepeat :: Int
maxRepeat = 10

regexSeries :: (Monad m) => Text -> Series m Text
regexSeries regexp =
  case parseRegex . unpack $ regexp of
    Right (p, _) -> toStrict . toLazyText <$> regexSeries' p
    Left _ -> pure ""

regexSeries' :: (Monad m) => Pattern -> Series m Builder
regexSeries' p =
  case p of
    PEmpty -> pure mempty
    PChar {..} -> pure $ singleton getPatternChar
    PAny {getPatternSet = PatternSet (Just cset) _ _ _} -> do
      x <- generate $ \depth -> take depth $ S.toList cset
      pure $ singleton x
    PAnyNot {getPatternSet = PatternSet (Just cset) _ _ _} -> do
      x <-
        generate $ \depth ->
          take depth $ notChars $ concatMap expandEscape $ S.toList cset
      pure $ singleton x
    PQuest p -> regexSeries' p \/ pure mempty
    PPlus p -> regexSeries' $ PBound 1 Nothing p
    PStar _ p -> regexSeries' $ PBound 0 Nothing p
    PBound low mhigh p -> do
      let high = fromMaybe (low + maxRepeat) mhigh
      n <- generate $ \depth -> take depth [low .. high]
      decDepth $ do
        ps <- replicateM n $ regexSeries' p
        pure $ mconcat ps
    PConcat ps -> mconcat <$> mapM regexSeries' ps
    POr xs -> regexSeries' =<< (generate $ \depth -> take depth xs)
    PDot _ -> do
      x <- generate $ \depth -> take depth $ notChars []
      pure $ singleton x
    PEscape {..} -> do
      x <- generate $ \depth -> take depth $ expandEscape getPatternChar
      pure $ singleton x
    PCarat _ -> pure mempty
    PDollar _ -> pure mempty
    _ -> pure mempty
  where
    notChars = ([' ' .. '~'] \\)
    expandEscape ch =
      case ch of
        'n' -> "\n"
        't' -> "\t"
        'r' -> "\r"
        'f' -> "\f"
        'a' -> "\a"
        'e' -> "\ESC"
        'd' -> ['0' .. '9']
        'w' -> ['0' .. '9'] ++ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']
        's' -> "\9\32"
        'D' -> notChars $ ['0' .. '9']
        'W' -> notChars $ ['0' .. '9'] ++ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']
        'S' -> notChars "\9\32"
        ch -> [ch]
