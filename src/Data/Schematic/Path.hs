module Data.Schematic.Path where

import Data.Singletons
import Data.Singletons.TypeLits
import Data.Schematic.Utils
import Data.Singletons.Prelude
import Data.Text as T


data PathSegment = Key Symbol | Ix Nat

data instance Sing (jp :: PathSegment) where
  SKey :: (KnownSymbol k, Known (Sing k)) => Sing (k :: Symbol) -> Sing (Key k)
  SIx :: (KnownNat n, Known (Sing n)) => Sing (n :: Nat) -> Sing (Ix n)

instance (KnownSymbol k, Known (Sing k))
  => Known (Sing (Key k)) where
  known = SKey known

instance (KnownNat n, Known (Sing n))
  => Known (Sing (Ix n)) where
  known = SIx known

pathToText :: Sing (ps :: [PathSegment]) -> Text
pathToText = T.pack . go ""
  where
    go :: String -> Sing (ps :: [PathSegment]) -> String
    go acc SNil = acc
    go acc (SCons (SKey k) tl) = go (acc ++ "." ++ symbolVal k) tl
    go acc (SCons (SIx n) tl) = go (acc ++ "[" ++ show (natVal n) ++ "]") tl
