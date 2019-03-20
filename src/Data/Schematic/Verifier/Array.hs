module Data.Schematic.Verifier.Array where

import Data.Schematic.Compat
import Data.Schematic.Constraints
import Data.Schematic.Verifier.Common


data VerifiedArrayConstraint =
  VAEq DeNat
  deriving (Show)

verifyArrayConstraint
  :: [ArrayConstraintT] -> Maybe (Maybe VerifiedArrayConstraint)
verifyArrayConstraint cs = fmap VAEq <$> verifyNEq [x | AEq x <- cs]
