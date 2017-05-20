{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Schematic.Utils where

import Data.Kind
import Data.Proxy
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Data.Vinyl hiding (Dict)


class Known a where
  known :: a

instance KnownNat n => Known (Sing n) where
  known = SNat

instance KnownSymbol s => Known (Sing s) where
  known = SSym

instance (Known (Sing a), Known (Sing b)) => Known (Sing '(a,b)) where
  known = STuple2 known known

instance Known (Sing '[]) where known = SNil

instance (Known (Sing a), Known (Sing as)) => Known (Sing (a ': as)) where
  known = SCons known known

instance Known (Rec Sing '[]) where
  known = RNil

instance (Known (Sing a), Known (Rec Sing tl)) => Known (Rec Sing (a ': tl)) where
  known = known :& known

data Dict c where
  Dict :: c => Dict c
