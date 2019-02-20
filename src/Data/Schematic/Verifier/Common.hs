module Data.Schematic.Verifier.Common where

import Data.List (nub)

simplifyNumberConstraint :: ([Integer] -> Integer) -> [Integer] -> Maybe Integer
simplifyNumberConstraint f =
  \case
    [] -> Nothing
    x -> Just $ f x

simplifyDNLs :: [Integer] -> Maybe Integer
simplifyDNLs = simplifyNumberConstraint minimum

simplifyDNGs :: [Integer] -> Maybe Integer
simplifyDNGs = simplifyNumberConstraint maximum

verifyDNEq :: [Integer] -> Maybe (Maybe Integer)
verifyDNEq x =
  case nub x of
    []      -> Just Nothing
    [y]     -> Just $ Just y
    (_:_:_) -> Nothing

verify3 :: Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe ()
verify3 (Just x) (Just y) (Just z) =
  if x < y && y < z
    then Just ()
    else Nothing
verify3 _ _ _ = Just ()

verify2 :: Maybe Integer -> Maybe Integer -> Maybe ()
verify2 (Just x) (Just y) =
  if x < y
    then Just ()
    else Nothing
verify2 _ _ = Just ()

verifyEquations :: Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe ()
verifyEquations mgt meq mlt = do
  verify3 mgt meq mlt
  verify2 mgt meq
  verify2 meq mlt
  verify2 mgt mlt
