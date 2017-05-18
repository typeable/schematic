module Data.Aeson.Validation where


import Control.Applicative
import Control.Monad
import Data.Aeson.Types as J
import Data.Foldable as F
import Data.Functor.Foldable
import Data.HashMap.Strict as H
import Data.Maybe
import Data.Proxy
import Data.Schematic.Validation
import Data.Singletons
import Data.Singletons.Decide
import Data.Text as T
import Data.Traversable


(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

infixl 3 <&&>

-- validateConstraint
--   :: TopLevel spec
--   => Sing spec
--   -> CRepr spec
--   -> Value
--   -> Maybe Bool
-- validateConstraint s c = \case
--   J.String t -> case s %~ SJText of
--     Proved Refl -> pure $ case fromSing c of
--       TEq n -> (== n) . fromIntegral . T.length $ t
--       TLe n -> (<= n) . fromIntegral . T.length $ t
--       TGt n -> (> n) . fromIntegral . T.length $ t
--     Disproved _ -> mzero
--   J.Number i -> case s %~ SJNumber of
--     Proved Refl -> pure $ case fromSing c of
--       NEq n -> i == fromIntegral n
--       NLe n -> i <= fromIntegral n
--       NGt n -> i > fromIntegral n
--     Disproved _ -> mzero
--   J.Array v -> case s %~ SJArray of
--     Proved Refl -> case fromSing c of
--       AEq n -> pure . (== fromIntegral n) . F.length $ v
--     Disproved _ -> Nothing
--   J.Null    -> Just True
--   _         -> Just False

-- validateConstraints
--   :: SDecide JType
--   => Sing jtype
--   -> [CRepr jtype]
--   -> Value
--   -> Maybe Bool
-- validateConstraints s cs v = and <$> traverse alg cs
--   where
--     alg c = validateConstraint s c v

-- validateJson
--   :: (SDecide JType, TopLevel spec)
--   => Sing spec
--   -> Value
--   -> Maybe Bool
-- validateJson = validateBySchema . fromSing

-- validateBySchema
--   :: SDecide JType
--   => Schema
--   -> Value
--   -> Maybe Bool
-- validateBySchema s v = case (unfix s, v) of
--   (SchemaArray cs ss, jv@(J.Array v)) -> do
--     itself <- validateConstraints SJArray cs jv
--     content <- traverse (validateBySchema ss) v
--     pure $ itself && and content
--   (SchemaObject ocs, J.Object o)   -> do
--     fmap and $ for ocs $ \(fieldName, s) -> do
--       val <- H.lookup (T.pack fieldName) o
--       validateBySchema s val
--   (SchemaText cs, js@(J.String _)) -> validateConstraints SJText cs js
--   (SchemaNumber cs, jn@(J.Number _)) -> validateConstraints SJNumber cs jn
--   (SchemaNull, J.Null)             -> Just True
--   _                                  -> Nothing
