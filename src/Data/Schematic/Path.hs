module Data.Schematic.Path where

import Data.Foldable as F
import Data.Monoid
import Data.Schematic.Utils
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Data.Text as T


data PathSegment = Key Symbol | Ix Nat

data instance Sing (jp :: PathSegment) where
  SKey :: (KnownSymbol k, Known (Sing k)) => Sing (k :: Symbol) -> Sing ('Key k)
  SIx :: (KnownNat n, Known (Sing n)) => Sing (n :: Nat) -> Sing ('Ix n)

instance (KnownSymbol k, Known (Sing k))
  => Known (Sing ('Key k)) where
  known = SKey known

instance (KnownNat n, Known (Sing n))
  => Known (Sing ('Ix n)) where
  known = SIx known

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
    go acc (SCons p ps) = go (acc ++ [demote p]) ps
    demote :: Sing (ps :: PathSegment) -> DemotedPathSegment
    demote (SKey s) = DKey $ T.pack $ symbolVal s
    demote (SIx n) = DIx $ natVal n

demotedPathToText :: [DemotedPathSegment] -> JSONPath
demotedPathToText = JSONPath . F.foldl' renderPathSegment ""
  where
    renderPathSegment acc (DKey t) = acc <> "." <> t
    renderPathSegment acc (DIx n)  = acc <> "[" <> T.pack (show n) <> "]"

pathToText :: Sing (ps :: [PathSegment]) -> JSONPath
pathToText = demotedPathToText . demotePath
