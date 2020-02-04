module Data.Schematic.Path where

import Data.Foldable as F
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Data.Text as T
import Data.Aeson.Internal


newtype JSONPathText = JSONPathText Text
  deriving (Show)

data PathSegment = PathKey Symbol | PathIx Nat

data instance Sing (jp :: PathSegment) where
  SKey :: (SingI k) => Sing (k :: Symbol) -> Sing ('PathKey k)
  SIx :: (SingI n) => Sing (n :: Nat) -> Sing ('PathIx n)

demotePath :: Sing (ps :: [PathSegment]) -> JSONPath
demotePath = go []
  where
    go :: JSONPath -> Sing (ps :: [PathSegment]) -> JSONPath
    go acc SNil = acc
    go acc (SCons p ps) = go (acc ++ [demotePathSeg p]) ps
    demotePathSeg :: Sing (ps :: PathSegment) -> JSONPathElement
    demotePathSeg (SKey s) = Key $ T.pack $ withKnownSymbol s $ symbolVal s
    demotePathSeg (SIx n) = Index $ withKnownNat n $ fromIntegral $ natVal n

demotedPathToText :: JSONPath -> JSONPathText
demotedPathToText = JSONPathText . F.foldl' renderPathSegment ""
  where
    renderPathSegment acc (Key t) = acc <> "." <> t
    renderPathSegment acc (Index n)  = acc <> "[" <> T.pack (show n) <> "]"

pathToText :: Sing (ps :: [PathSegment]) -> JSONPathText
pathToText = demotedPathToText . demotePath
