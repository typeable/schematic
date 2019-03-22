{-# LANGUAGE CPP #-}
module Data.Schematic.Path where

import Data.Foldable as F
import Data.Schematic.Compat
import Data.Singletons.Prelude
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Text as T
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif


singletons [d|
  data PathSegment' s n = Key s | Ix n
    deriving Show
  |]

type PathSegment = PathSegment' Symbol Nat
type DemotedPathSegment = PathSegment' Text DeNat

-- | Textual representation of json path.
newtype JSONPath = JSONPath Text
  deriving (Show)

demotedPathToText :: [DemotedPathSegment] -> JSONPath
demotedPathToText = JSONPath . F.foldl' renderPathSegment ""
  where
    renderPathSegment acc (Key t) = acc <> "." <> t
    renderPathSegment acc (Ix n)  = acc <> "[" <> T.pack (show n) <> "]"

pathToText :: Sing (ps :: [PathSegment]) -> JSONPath
pathToText = demotedPathToText . fromSing
