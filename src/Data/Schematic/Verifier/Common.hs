module Data.Schematic.Verifier.Common where

import Data.List (nub)
import Data.Schematic.Compat


simplifyNumberConstraint :: ([DeNat] -> DeNat) -> [DeNat] -> Maybe DeNat
simplifyNumberConstraint f =
  \case
    [] -> Nothing
    x -> Just $ f x

simplifyNLs :: [DeNat] -> Maybe DeNat
simplifyNLs = simplifyNumberConstraint minimum

simplifyNGs :: [DeNat] -> Maybe DeNat
simplifyNGs = simplifyNumberConstraint maximum

verifyNEq :: [DeNat] -> Maybe (Maybe DeNat)
verifyNEq x =
  case nub x of
    []      -> Just Nothing
    [y]     -> Just $ Just y
    (_:_:_) -> Nothing

verify3 :: Maybe DeNat -> Maybe DeNat -> Maybe DeNat -> Maybe ()
verify3 (Just x) (Just y) (Just z) =
  if x < y && y < z
    then Just ()
    else Nothing
verify3 _ _ _ = Just ()

verify2 :: Maybe DeNat -> Maybe DeNat -> Maybe ()
verify2 (Just x) (Just y) =
  if x < y
    then Just ()
    else Nothing
verify2 _ _ = Just ()

verifyEquations :: Maybe DeNat -> Maybe DeNat -> Maybe DeNat -> Maybe ()
verifyEquations mgt meq mlt = do
  verify3 mgt meq mlt
  verify2 mgt meq
  verify2 meq mlt
  verify2 mgt mlt
