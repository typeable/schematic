module Data.Schematic.Verifier.Common where

import Data.List (nub)
import GHC.Natural


simplifyNumberConstraint :: ([Natural] -> Natural) -> [Natural] -> Maybe Natural
simplifyNumberConstraint f =
  \case
    [] -> Nothing
    x -> Just $ f x

simplifyNLs :: [Natural] -> Maybe Natural
simplifyNLs = simplifyNumberConstraint minimum

simplifyNGs :: [Natural] -> Maybe Natural
simplifyNGs = simplifyNumberConstraint maximum

verifyNEq :: [Natural] -> Maybe (Maybe Natural)
verifyNEq x =
  case nub x of
    []      -> Just Nothing
    [y]     -> Just $ Just y
    (_:_:_) -> Nothing

verify3 :: Maybe Natural -> Maybe Natural -> Maybe Natural -> Maybe ()
verify3 (Just x) (Just y) (Just z) =
  if x < y && y < z
    then Just ()
    else Nothing
verify3 _ _ _ = Just ()

verify2 :: Maybe Natural -> Maybe Natural -> Maybe ()
verify2 (Just x) (Just y) =
  if x < y
    then Just ()
    else Nothing
verify2 _ _ = Just ()

verifyEquations :: Maybe Natural -> Maybe Natural -> Maybe Natural -> Maybe ()
verifyEquations mgt meq mlt = do
  verify3 mgt meq mlt
  verify2 mgt meq
  verify2 meq mlt
  verify2 mgt mlt
