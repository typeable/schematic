{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Schematic.DSL where

import           Data.Kind
import           Data.Schematic.Lens
import           Data.Schematic.Migration
import           Data.Schematic.Schema
import           Data.Scientific
import           Data.Singletons
import           Data.Singletons.Prelude hiding ((:.))
import           Data.Singletons.TypeLits
import           Data.Tagged
import           Data.Text as T
import           Data.Union
import qualified Data.Vector as V
import           Data.Vinyl
import           Data.Vinyl.Functor


type Constructor a
  = forall b. FSubset (FieldsOf a) b (FImage (FieldsOf a) b)
  => Rec (Tagged (FieldsOf a) :. FieldRepr) b
  -> JsonRepr ('SchemaObject (FieldsOf a))

withRepr :: Constructor a
withRepr = ReprObject . rmap (unTagged . getCompose) . fcast

class Representable s where
  type Repr s :: Type
  construct :: Sing fn -> Proxy s -> Repr s -> FieldRepr '(fn, s)

instance SingI so => Representable ('SchemaObject so) where
  type Repr ('SchemaObject so) = Rec FieldRepr so
  construct sfn _ o = withKnownSymbol sfn $ FieldRepr $ ReprObject o

instance (SingI cs, SingI sa) => Representable ('SchemaArray cs sa) where
  type Repr ('SchemaArray cs sa) = V.Vector (JsonRepr sa)
  construct sfn _ a = withKnownSymbol sfn $ FieldRepr $ ReprArray a

instance SingI cs => Representable ('SchemaText cs) where
  type Repr ('SchemaText cs) = Text
  construct sfn _ t = withKnownSymbol sfn $ FieldRepr $ ReprText t

instance SingI cs => Representable ('SchemaNumber cs) where
  type Repr ('SchemaNumber cs) = Scientific
  construct sfn _ n = withKnownSymbol sfn $ FieldRepr $ ReprNumber n

instance Representable 'SchemaBoolean where
  type Repr 'SchemaBoolean = Bool
  construct sfn _ b = withKnownSymbol sfn $ FieldRepr $ ReprBoolean b

instance SingI so => Representable ('SchemaOptional so) where
  type Repr ('SchemaOptional so) = Maybe (JsonRepr so)
  construct sfn _ o = withKnownSymbol sfn $ FieldRepr $ ReprOptional o

instance SingI (h ': tl) => Representable ('SchemaUnion (h ': tl)) where
  type Repr ('SchemaUnion (h ': tl)) = Union JsonRepr (h ': tl)
  construct sfn _ u = withKnownSymbol sfn $ FieldRepr $ ReprUnion u

type family FieldsOf (s :: Schema) :: [(Symbol, Schema)] where
  FieldsOf ('SchemaObject fs) = fs

type FieldConstructor fn =
  forall fs. (Representable (ByField fn fs (FIndex fn fs)))
  => Repr (ByField fn fs (FIndex fn fs))
  -> (Tagged fs :. FieldRepr) '(fn, (ByField fn fs (FIndex fn fs)))

field :: forall fn. KnownSymbol fn => FieldConstructor fn
field = Compose . Tagged . construct (sing :: Sing fn) Proxy
