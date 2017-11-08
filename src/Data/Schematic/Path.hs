module Data.Schematic.Path where

import Data.Foldable as F
import Data.Monoid
import Data.Schematic.Schema
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Data.Text as T


-- Schema level path

data Path
  = PKey Symbol -- traverse into the object by key
  | PTraverse   -- traverse into the array

data instance Sing (p :: Path) where
  SPKey :: Sing s -> Sing ('PKey s)
  SPTraverse :: Sing 'PTraverse

instance KnownSymbol s => SingI ('PKey s) where
  sing = SPKey sing

instance SingI 'PTraverse where
  sing = SPTraverse

type family TraverseStep (p :: Path) (s :: Schema) :: Schema where
  TraverseStep ('PKey fn) ('SchemaObject ( '(fn, s) ': tl )) = s
  TraverseStep ('PKey fn) ('SchemaObject ( '(x, s) ': tl )) =
    TraverseStep ('PKey fn) ('SchemaObject tl)
  TraverseStep 'PTraverse ('SchemaArray cs s) = s

type family TraversePath (ps :: [Path]) (s :: Schema) :: Schema where
  TraversePath '[] s = s
  TraversePath (p ': ps) s = TraversePath ps (TraverseStep p s)

-- Data level path

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
    go acc (SCons p ps) = go (acc ++ [demote p]) ps
    demote :: Sing (ps :: PathSegment) -> DemotedPathSegment
    demote (SKey s) = DKey $ T.pack $ withKnownSymbol s $ symbolVal s
    demote (SIx n) = DIx $ withKnownNat n $ natVal n

demotedPathToText :: [DemotedPathSegment] -> JSONPath
demotedPathToText = JSONPath . F.foldl' renderPathSegment ""
  where
    renderPathSegment acc (DKey t) = acc <> "." <> t
    renderPathSegment acc (DIx n)  = acc <> "[" <> T.pack (show n) <> "]"

pathToText :: Sing (ps :: [PathSegment]) -> JSONPath
pathToText = demotedPathToText . demotePath
