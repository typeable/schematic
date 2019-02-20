module Data.Schematic.Schema where

import           Data.Kind
import           Data.Maybe
import           Data.Schematic.Instances ()
import           Data.Scientific
import           Data.Singletons.TH
import           Data.Singletons.TypeLits
import           Data.Text as T
import           Data.Union
import           Data.Vector as V
import           Data.Vinyl hiding (Dict)
import           Prelude as P

data TextConstraint
  = TEq Nat
  | TLt Nat
  | TLe Nat
  | TGt Nat
  | TGe Nat
  | TRegex Symbol
  | TEnum [Symbol]

data DemotedTextConstraint
  = DTEq Integer
  | DTLt Integer
  | DTLe Integer
  | DTGt Integer
  | DTGe Integer
  | DTRegex Text
  | DTEnum [Text]

data NumberConstraint
  = NLe Nat
  | NLt Nat
  | NGt Nat
  | NGe Nat
  | NEq Nat

data DemotedNumberConstraint
  = DNLe Integer
  | DNLt Integer
  | DNGt Integer
  | DNGe Integer
  | DNEq Integer

data ArrayConstraint
  = AEq Nat

data DemotedArrayConstraint
  = DAEq Integer

data Schema
  = SchemaText [TextConstraint]
  | SchemaBoolean
  | SchemaNumber [NumberConstraint]
  | SchemaObject [(Symbol, Schema)]
  | SchemaArray [ArrayConstraint] Schema
  | SchemaNull
  | SchemaOptional Schema
  | SchemaUnion [Schema]

data DemotedSchema
  = DSchemaText [DemotedTextConstraint]
  | DSchemaNumber [DemotedNumberConstraint]
  | DSchemaBoolean
  | DSchemaObject [(Text, DemotedSchema)]
  | DSchemaArray [DemotedArrayConstraint] DemotedSchema
  | DSchemaNull
  | DSchemaOptional DemotedSchema
  | DSchemaUnion [DemotedSchema]

data FieldRepr :: (Symbol, Schema) -> Type where
  FieldRepr
    :: (SingI schema, KnownSymbol name)
    => JsonRepr schema
    -> FieldRepr '(name, schema)

data JsonRepr :: Schema -> Type where
  ReprText :: Text -> JsonRepr ('SchemaText cs)
  ReprNumber :: Scientific -> JsonRepr ('SchemaNumber cs)
  ReprBoolean :: Bool -> JsonRepr 'SchemaBoolean
  ReprNull :: JsonRepr 'SchemaNull
  ReprArray :: V.Vector (JsonRepr s) -> JsonRepr ('SchemaArray cs s)
  ReprObject :: Rec FieldRepr fs -> JsonRepr ('SchemaObject fs)
  ReprOptional :: Maybe (JsonRepr s) -> JsonRepr ('SchemaOptional s)
  ReprUnion :: Union JsonRepr (h ': tl) -> JsonRepr ('SchemaUnion (h ': tl))
