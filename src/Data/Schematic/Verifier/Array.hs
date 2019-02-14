module Data.Schematic.Verifier.Array where

import Data.List
import Data.Maybe
import Data.Schematic
import Data.Schematic.Verifier.Common
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Data.Text (Text)
import Data.Text.Array (Array)
import GHC.Generics

data VerifiedArrayConstraint =
  VAEq Integer
  deriving (Show)

verifyArrayConstraint ::
     [DemotedArrayConstraint] -> Maybe (Maybe VerifiedArrayConstraint)
verifyArrayConstraint cs = do
  x <- verifyDNEq [x | DAEq x <- cs]
  pure $ VAEq <$> x
