{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Schematic.Instances where

import Data.Scientific
import Data.Text (Text, pack)
import Data.Vector as V
import Data.Vinyl
import Test.SmallCheck.Series


instance Monad m => Serial m (Rec f '[]) where
  series = cons0 RNil

instance (Serial m (f a), Serial m (Rec f as), Monad m)
  => Serial m (Rec f (a ': as)) where
  series = cons2 (:&)

instance Serial m a => Serial m (V.Vector a) where
  series = V.fromList <$> series

instance Monad m => Serial m Scientific where
  series = scientific <$> series <*> series

instance Monad m => Serial m Text where
  series = pack <$> series
