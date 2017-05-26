{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Schematic.Migration where

import Data.Kind
import Data.Schematic.Path
import Data.Schematic.Schema
import Data.Schematic.Utils
import Data.Singletons.Prelude hiding (All)
import Data.Singletons.TypeLits
import Data.Vinyl

data Path
  = PKey Symbol -- traverse into the object by key
  | PTraverse   -- traverse into the array

data instance Sing (p :: Path) where
  SPKey :: KnownSymbol s => Sing s -> Sing ('PKey s)
  SPTraverse :: Sing 'PTraverse

-- Result type blueprint
data Builder
  = BKey Schema Symbol Build  -- field schema
  | BTraverse Schema Builder  -- array schema
  | BScalar Schema

-- Working with keys

type family SchemaByKey (fs :: [(Symbol, Schema)]) (s :: Symbol) :: Schema where
  SchemaByKey ( '(a, s) ': tl) fn = SchemaByKey tl fn
  SchemaByKey ( '(fn,s) ': tl) fn = s

type family DeleteKey (acc :: [(Symbol, Schema)]) (fn :: Symbol) (fs :: [(Symbol, Schema)]) :: Schema where
  DeleteKey acc fn ('(fn, a) ': tl) = acc :++ tl
  DeleteKey acc fn (fna ': tl) = acc :++ (fna ': tl)

type family UpdateKey (acc :: [(Symbol, Schema)]) (fn :: Symbol) (fs :: [(Symbol, Schema)]) (s :: Schema) :: [(Symbol, Schema)] where
  UpdateKey acc fn ( '(fn, n) ': tl ) s = '(fn, s) ': tl
  UpdateKey acc fn ( '(a, n) ': tl) s = acc :++ '(a, n) ': UpdateKey fn tl

type family Build (z :: Builder) :: Schema where
  Build ('BKey (SchemaObject fs) fn z) = UpdateKey '[] fn fs (Build z)
  Build ('BTraverse (SchemaArray acs s) z) = SchemaArray acs (Build z)
  Build ('BScalar s) = s

type family MakeBuilder (s :: Schema) (d :: Diff) :: Builder where
  MakeBuilder s ('Diff '[] a) = BScalar (ApplyAction a s)
  MakeBuilder ('SchemaObject fs) ('Diff ('PKey fn ': tl) a) =
    BKey ('SchemaObject fs) fn (MakeBuilder (SchemaByKey fn fs) ('Diff tl a))
  MakeBuilder ('SchemaArray acs s) ('Diff ('PTraverse ': tl) a) =
    MakeBuilder ('SchemaArray acs s) (MakeBuilder s ('Diff tl a))

type family ApplyAction (a :: Action) (s :: Schema) :: Schema where
  ApplyAction ('AddKey fn s) ('SchemaObject fs) = 'SchemaObject ('(fn,s) ': fs)
  ApplyAction ('DeleteKey fn) ('SchemaObject fs) = 'SchemaObject (DeleteKey fn fs)
  ApplyAction ('Update s) t = s

data instance Sing (z :: Builder) where
  SZipper
    :: (Known (Sing ts), Known (Sing s))
    => Sing ts
    -> Sing s
    -> Sing (Builder ts s)

class MigrateSchema (a :: Schema) (b :: Schema) where
  migrate :: JsonRepr a -> JsonRepr b

data Action = AddKey Symbol Schema | Update Schema | DeleteKey Symbol

data instance Sing (a :: Action) where
  SAddKey
    :: (Known (Sing n), Known (Sing s))
    => Sing n
    -> Sing s
    -> Sing ('AddKey n s)
  SUpdate :: Known (Sing s) => Sing s -> Sing ('Update s)
  SDelete :: Sing 'DeleteKey

instance (Known (Sing n), Known (Sing s)) => Known (Sing ('AddKey n s)) where
  known = SAddKey known known
instance (Known (Sing s)) => Known (Sing ('Update s)) where known = SUpdate known
instance Known (Sing 'DeleteKey) where known = SDelete

-- | User-supplied atomic difference between schemas.
-- Migrations can consists of many differences.
data Diff = Diff [PathSegment] Action

data instance Sing (diff :: Diff) where
  SDiff
    :: (Known (Sing jp), Known (Sing a))
    => Sing (jp :: [PathSegment])
    -> Sing (a :: Action)
    -> Sing ('Diff jp a)

-- | User-provided name of the revision.
type Revision = Symbol

data Migration = Migration Revision [Diff]

-- type family TypedMigration (s :: Schema) (m :: Migration) :: Constraint where
--   TypedMigration s ('Migration r ds) = (TypedDiffList s ds)

-- type family TypedMigrationSchema (s :: Schema) (m :: Migration) :: (Symbol, Schema) where
--   TypedMigrationSchema s ('Migration r ds) = '(r, TypedDiffListSchema s ds)

data instance Sing (m :: Migration) where
  SMigration
    :: (KnownSymbol r, Known (Sing ds))
    => Sing r
    -> Sing ds
    -> Sing ('Migration r ds)

data Versioned = Versioned Schema [Migration]

-- type family TypedVersioned (s :: Schema) (ms :: [Migration]) :: Constraint where
--   TypedVersioned s '[]       = ()
--   TypedVersioned s (h ': tl) =
--     ( TypedMigration s h
--     , MigrateSchema s (Snd (TypedMigrationSchema s h))
--     , TypedVersioned (Snd (TypedMigrationSchema s h)) tl )

type family TypedVersionedSchema

data instance Sing (v :: Versioned) where
  SVersioned
    :: (Known (Sing s), Known (Sing ms), TypedMigration s m)
    => Sing (s :: Schema)  -- base version
    -> Sing (ms :: [Migration]) -- a bunch of migrations
    -> Sing ('Versioned s ms)

type family HeadVersion (vd :: Versioned) :: Schema where
  HeadVersion ('Versioned s '[])        = s
  HeadVersion ('Versioned s (m ': tl))  =
    HeadVersion ('Versioned (Snd (TypedMigrationSchema s m)) tl)

type SchemaExample
  = 'SchemaObject
    '[ '("foo", 'SchemaArray '[ 'AEq 1] ( 'SchemaNumber '[ 'NGt 10 ]))
     , '("bar", 'SchemaOptional ( 'SchemaText '[ 'TRegex "\\w+", 'TEnum '["foo", "bar"]]))]

type VS =
  'Versioned SchemaExample
    '[ 'Migration "test_revision"
       '[ 'Diff '[ 'Key "foo" ] ('Update ('SchemaText '[])) ] ]

-- jsonExample :: JsonRepr (HeadVersion VS)
-- jsonExample = ReprObject $
--   FieldRepr (ReprText "foo")
--     :& FieldRepr (ReprOptional (Just (ReprText "bar")))
--     :& Nil
