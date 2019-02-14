module Data.Schematic.Verifier.Text where

import Control.Monad
import Data.Maybe
import Data.Schematic
import Data.Schematic.Verifier.Common
import Data.Text (Text, unpack)
import Text.Regex.TDFA.Pattern
import Text.Regex.TDFA.ReadRegex (parseRegex)

toStrictTextLength :: [DemotedTextConstraint] -> [DemotedTextConstraint]
toStrictTextLength = map f
  where
    f (DTLe x) = DTLt (x + 1)
    f (DTGe x) = DTGt (x - 1)
    f x = x

data VerifiedTextConstraint
  = VTEq Integer
  | VTBounds (Maybe Integer)
             (Maybe Integer)
  | VTRegex Text
            Integer
            (Maybe Integer)
  | VTEnum [Text]
  deriving (Show)

verifyTextLengthConstraints ::
     [DemotedTextConstraint] -> Maybe (Maybe VerifiedTextConstraint)
verifyTextLengthConstraints cs' = do
  let cs = toStrictTextLength cs'
      mlt = simplifyDNLs [x | DTLt x <- cs]
      mgt = simplifyDNGs [x | DTGt x <- cs]
  meq <- verifyDNEq [x | DTEq x <- cs]
  verifyEquations mgt meq mlt
  case all isNothing ([mgt, meq, mlt] :: [Maybe Integer]) of
    True -> Just Nothing
    _ ->
      Just $
      Just $
      case meq of
        Just eq -> VTEq eq
        Nothing -> VTBounds mgt mlt

regexLength :: Text -> Maybe (Int, Maybe Int)
regexLength regexp =
  case parseRegex . unpack $ regexp of
    Right (p, _) -> Just (minRegexLength p, maxRegexLength p)
    Left _ -> Nothing

minRegexLength :: Pattern -> Int
minRegexLength p =
  case p of
    PEmpty -> 0
    PChar {..} -> 1
    PAny {..} -> 1
    PAnyNot {..} -> 1
    PQuest _ -> 0
    PPlus sch -> minRegexLength $ PBound 1 Nothing sch
    PStar _ sch -> minRegexLength $ PBound 0 Nothing sch
    PBound low _ sch -> low * minRegexLength sch
    PConcat ps -> sum $ fmap minRegexLength ps
    POr xs -> minimum $ fmap minRegexLength xs
    PDot _ -> 1
    PEscape {..} -> 1
    PCarat _ -> 0
    PDollar _ -> 0
    _ -> 0

maxRegexLength :: Pattern -> Maybe Int
maxRegexLength p =
  case p of
    PEmpty -> Just 0
    PChar _ _ -> Just 1
    PAny _ _ -> Just 1
    PAnyNot _ _ -> Just 1
    PQuest _ -> Just 0
    PPlus _ -> Nothing
    PStar _ _ -> Nothing
    PBound _ mhigh sch -> (*) <$> mhigh <*> maxRegexLength sch
    PConcat ps -> sum <$> mapM maxRegexLength ps
    POr xs -> maximum <$> mapM maxRegexLength xs
    PDot _ -> Just 1
    PEscape _ _ -> Just 1
    PCarat _ -> Just 0
    PDollar _ -> Just 0
    _ -> Just 0

verifyTextRegexConstraint ::
     [DemotedTextConstraint] -> Maybe (Maybe VerifiedTextConstraint)
verifyTextRegexConstraint cs = do
  let regexps = [x | DTRegex x <- cs]
  case regexps of
    [] -> Just Nothing
    [x] -> do
      (l, mh) <- regexLength x
      Just $ Just $ VTRegex x (fromIntegral l) (fromIntegral <$> mh)
    _ -> Nothing

verifyTextEnumConstraint ::
     [DemotedTextConstraint] -> Maybe (Maybe VerifiedTextConstraint)
verifyTextEnumConstraint cs = do
  let enums = concat [x | DTEnum x <- cs]
  case enums of
    [] -> Just Nothing
    x -> Just $ Just $ VTEnum x

verifyTextConstraints ::
     [DemotedTextConstraint] -> Maybe [VerifiedTextConstraint]
verifyTextConstraints cs = do
  regexp <- verifyTextRegexConstraint cs
  void $ case regexp of
    Just (VTRegex _ l mh) ->
      verifyTextLengthConstraints (DTGe l : cs ++ maybeToList (DTLe <$> mh))
    _ -> pure Nothing
  lengths <- verifyTextLengthConstraints cs
  enums <- verifyTextEnumConstraint cs
  return $ catMaybes [lengths, enums, regexp]
