module Data.Schematic.Verifier.Number where

import Data.List
import Data.Maybe
import Data.Schematic
import Data.Schematic.Schema
import Data.Schematic.Verifier.Common
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Data.Text (Text)
import Data.Text.Array (Array)
import GHC.Generics

toStrictNumber :: [DemotedNumberConstraint] -> [DemotedNumberConstraint]
toStrictNumber = map f
  where
    f (DNLe x) = DNLt (x + 1)
    f (DNGe x) = DNGt (x - 1)
    f x = x

data VerifiedNumberConstraint
  = VNEq Integer
  | VNBounds (Maybe Integer)
             (Maybe Integer)
  deriving (Show)

verifyNumberConstraints ::
     [DemotedNumberConstraint]
  -> Maybe VerifiedNumberConstraint
verifyNumberConstraints cs' = do
  let cs = toStrictNumber cs'
      mlt = simplifyDNLs [x | DNLt x <- cs]
      mgt = simplifyDNGs [x | DNGt x <- cs]
  meq <- verifyDNEq [x | DNEq x <- cs]
  verifyEquations mgt meq mlt
  Just $
    case meq of
      Just eq -> VNEq eq
      Nothing -> VNBounds mgt mlt
