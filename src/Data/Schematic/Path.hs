module Data.Schematic.Path where

import Data.Foldable as F
import Data.Monoid
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Data.Text as T


data PathSegment = Key Symbol | Ix Nat

data instance Sing (jp :: PathSegment) where
  SKey :: (SingI k) => Sing (k :: Symbol) -> Sing ('Key k)
  SIx :: (SingI n) => Sing (n :: Nat) -> Sing ('Ix n)

data DemotedPathSegment = DKey Text | DIx Integer
  deriving (Show)

-- | Textual representation of json path.
newtype JSONPath = JSONPath Text
  deriving (Show)

demotePath :: Sing (ps :: [PathSegment]) -> [DemotedPathSegment]
demotePath = go []
  where
    go :: [DemotedPathSegment] -> Sing (ps :: [PathSegment]) -> [DemotedPathSegment]
    go acc SNil = acc
    go acc (SCons p ps) = go (acc ++ [demotePathSeg p]) ps
    demotePathSeg :: Sing (ps :: PathSegment) -> DemotedPathSegment
    demotePathSeg (SKey s) = DKey $ T.pack $ withKnownSymbol s $ symbolVal s
    demotePathSeg (SIx n) = DIx $ withKnownNat n $ fromIntegral $ natVal n

demotedPathToText :: [DemotedPathSegment] -> JSONPath
demotedPathToText = JSONPath . F.foldl' renderPathSegment ""
  where
    renderPathSegment acc (DKey t) = acc <> "." <> t
    renderPathSegment acc (DIx n)  = acc <> "[" <> T.pack (show n) <> "]"

pathToText :: Sing (ps :: [PathSegment]) -> JSONPath
pathToText = demotedPathToText . demotePath
