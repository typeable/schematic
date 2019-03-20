module Data.Schematic.Verifier.Array where

import Data.Schematic.Constraints
import Data.Schematic.Verifier.Common
import GHC.Natural


data VerifiedArrayConstraint =
  VAEq Natural
  deriving (Show)

verifyArrayConstraint
  :: [ArrayConstraintT] -> Maybe (Maybe VerifiedArrayConstraint)
verifyArrayConstraint cs = fmap VAEq <$> verifyNEq [x | AEq x <- cs]
