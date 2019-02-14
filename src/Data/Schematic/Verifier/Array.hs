module Data.Schematic.Verifier.Array where

import Data.Schematic
import Data.Schematic.Verifier.Common

data VerifiedArrayConstraint =
  VAEq Integer
  deriving (Show)

verifyArrayConstraint ::
     [DemotedArrayConstraint] -> Maybe (Maybe VerifiedArrayConstraint)
verifyArrayConstraint cs = do
  x <- verifyDNEq [x | DAEq x <- cs]
  pure $ VAEq <$> x
