{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Schematic.DSL where

import           Data.Kind
import           Data.Schematic.Compat
import           Data.Schematic.Lens
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
  = forall fields b
  . (fields ~ FieldsOf a, FSubset fields b (FImage fields b), RMapCompat fields)
  => Rec (Tagged fields :. FieldRepr) b
  -> JsonRepr ('SchemaObject fields)

withRepr :: Constructor a
withRepr = ReprObject . rmap (unTagged . getCompose) . fcast

class Representable s where
  constructField :: Sing fn -> Proxy s -> Repr s -> FieldRepr '(fn, s)

instance SingI so => Representable ('SchemaObject so) where
  constructField sfn _ o = withKnownSymbol sfn $ FieldRepr $ ReprObject o

instance (SingI cs, SingI sa) => Representable ('SchemaArray cs sa) where
  constructField sfn _ a = withKnownSymbol sfn $ FieldRepr $ ReprArray a

instance SingI cs => Representable ('SchemaText cs) where
  constructField sfn _ t = withKnownSymbol sfn $ FieldRepr $ ReprText t

instance SingI cs => Representable ('SchemaNumber cs) where
  constructField sfn _ n = withKnownSymbol sfn $ FieldRepr $ ReprNumber n

instance Representable 'SchemaBoolean where
  constructField sfn _ b = withKnownSymbol sfn $ FieldRepr $ ReprBoolean b

instance SingI so => Representable ('SchemaOptional so) where
  constructField sfn _ o = withKnownSymbol sfn $ FieldRepr $ ReprOptional o

instance SingI (h ': tl) => Representable ('SchemaUnion (h ': tl)) where
  constructField sfn _ u = withKnownSymbol sfn $ FieldRepr $ ReprUnion u

construct :: Sing s -> Repr s -> JsonRepr s
construct s r = case s of
  SSchemaObject _          -> ReprObject r
  SSchemaArray _ _         -> ReprArray r
  SSchemaText _            -> ReprText r
  SSchemaNumber _          -> ReprNumber r
  SSchemaBoolean           -> ReprBoolean r
  SSchemaOptional _        -> ReprOptional r
  SSchemaNull              -> ReprNull
  SSchemaUnion ss          -> case ss of
    SNil      -> error "unconstructable union"
    SCons _ _ -> ReprUnion r

type family FieldsOf (s :: Schema) :: [(Symbol, Schema)] where
  FieldsOf ('SchemaObject fs) = fs

type FieldConstructor fn =
  forall byField fs. (byField ~ ByField fn fs (FIndex fn fs), Representable byField)
  => Repr byField
  -> (Tagged fs :. FieldRepr) '(fn, byField)

field :: forall fn. KnownSymbol fn => FieldConstructor fn
field = Compose . Tagged . constructField (sing :: Sing fn) Proxy

type family Repr (s :: Schema) = (ty :: Type) where
  Repr ('SchemaObject so) = Rec FieldRepr so
  Repr ('SchemaArray cs sa) = V.Vector (JsonRepr sa)
  Repr ('SchemaText cs) = Text
  Repr ('SchemaNumber cs) = Scientific
  Repr 'SchemaBoolean = Bool
  Repr ('SchemaOptional so) = Maybe (JsonRepr so)
  Repr ('SchemaUnion (h ': tl)) = Union JsonRepr (h ': tl)
