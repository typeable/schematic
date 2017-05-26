{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Schematic.Migration where

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
  = BKey Schema Symbol Builder  -- field schema
  | BTraverse Schema Builder  -- array schema
  | BScalar Schema

-- Working with keys

type family SchemaByKey (fs :: [(Symbol, Schema)]) (s :: Symbol) :: Schema where
  SchemaByKey ( '(fn,s) ': tl) fn = s
  SchemaByKey ( '(a, s) ': tl) fn = SchemaByKey tl fn

type family DeleteKey (acc :: [(Symbol, Schema)]) (fn :: Symbol) (fs :: [(Symbol, Schema)]) :: [(Symbol, Schema)] where
  DeleteKey acc fn ('(fn, a) ': tl) = acc :++ tl
  DeleteKey acc fn (fna ': tl) = acc :++ (fna ': tl)

type family UpdateKey (acc :: [(Symbol, Schema)]) (fn :: Symbol) (fs :: [(Symbol, Schema)]) (s :: Schema) :: [(Symbol, Schema)] where
  UpdateKey acc fn ( '(fn, n) ': tl ) s = '(fn, s) ': tl
  UpdateKey acc fn ( '(a, n) ': tl) s = UpdateKey (acc :++ '[ '(a,n) ]) fn tl s

-- schema updates

type family Build (b :: Builder) :: Schema where
  Build ('BKey ('SchemaObject fs) fn z) = 'SchemaObject (UpdateKey '[] fn fs (Build z))
  Build ('BTraverse ('SchemaArray acs s) z) = 'SchemaArray acs (Build z)
  Build ('BScalar s) = s

type family MakeBuilder (s :: Schema) (d :: Diff) :: Builder where
  MakeBuilder s ('Diff '[] a) = 'BScalar (ApplyAction a s)
  MakeBuilder ('SchemaObject fs) ('Diff ('PKey fn ': tl) a) =
    'BKey ('SchemaObject fs) fn (MakeBuilder (SchemaByKey fs fn) ('Diff tl a))
  MakeBuilder ('SchemaArray acs s) ('Diff ('PTraverse ': tl) a) =
    'BTraverse ('SchemaArray acs s) (MakeBuilder s ('Diff tl a))

type family ApplyAction (a :: Action) (s :: Schema) :: Schema where
  ApplyAction ('AddKey fn s) ('SchemaObject fs) = 'SchemaObject ('(fn,s) ': fs)
  ApplyAction ('DeleteKey fn) ('SchemaObject fs) = 'SchemaObject (DeleteKey '[] fn fs)
  ApplyAction ('Update s) t = s

type family ApplyMigration (m :: Migration) (s :: Schema) :: (Revision, Schema) where
  ApplyMigration ('Migration r '[])       s = '(r, s)
  ApplyMigration ('Migration r (d ': ds)) s =
    '(r, Snd (ApplyMigration ('Migration r ds) (Build (MakeBuilder s d))))

-- working with revisions

type family SchemaByRevision (r :: Revision) (vd :: Versioned) :: Schema where
  SchemaByRevision r ('Versioned s (('Migration r ds) ': ms)) =
    Snd (ApplyMigration ('Migration r ds) s)
  SchemaByRevision r ('Versioned s (m ': ms)) =
    SchemaByRevision r ('Versioned (Snd (ApplyMigration m s)) ms)

type family TopVersion (vd :: Versioned) :: Schema where
  TopVersion ('Versioned s '[])       = s
  TopVersion ('Versioned s (m ': ms)) =
    TopVersion ('Versioned (Snd (ApplyMigration m s)) ms)

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
  SDeleteKey :: KnownSymbol s => Sing s -> Sing ('DeleteKey s)

instance (Known (Sing n), Known (Sing s)) => Known (Sing ('AddKey n s)) where
  known = SAddKey known known
instance (Known (Sing s)) => Known (Sing ('Update s)) where known = SUpdate known
instance (KnownSymbol s, Known (Sing s)) => Known (Sing ('DeleteKey s)) where
  known = SDeleteKey known

-- | User-supplied atomic difference between schemas.
-- Migrations can consists of many differences.
data Diff = Diff [Path] Action

data instance Sing (diff :: Diff) where
  SDiff
    :: (Known (Sing jp), Known (Sing a))
    => Sing (jp :: [Path])
    -> Sing (a :: Action)
    -> Sing ('Diff jp a)

-- | User-provided name of the revision.
type Revision = Symbol

data Migration = Migration Revision [Diff]

data instance Sing (m :: Migration) where
  SMigration
    :: (KnownSymbol r, Known (Sing ds))
    => Sing r
    -> Sing ds
    -> Sing ('Migration r ds)

data Versioned = Versioned Schema [Migration]

data instance Sing (v :: Versioned) where
  SVersioned
    :: (Known (Sing s), Known (Sing ms))
    => Sing (s :: Schema)  -- base version
    -> Sing (ms :: [Migration]) -- a bunch of migrations
    -> Sing ('Versioned s ms)

type SchemaExample
  = 'SchemaObject
    '[ '("foo", 'SchemaArray '[ 'AEq 1] ( 'SchemaNumber '[ 'NGt 10 ]))
     , '("bar", 'SchemaOptional ( 'SchemaText '[ 'TRegex "\\w+", 'TEnum '["foo", "bar"]]))]

type VS =
  'Versioned SchemaExample
    '[ 'Migration "test_revision"
       '[ 'Diff '[ 'PKey "foo" ] ('Update ('SchemaText '[])) ] ]

jsonExample :: JsonRepr (TopVersion VS)
jsonExample = ReprObject $
  FieldRepr (ReprText "foo")
    :& FieldRepr (ReprOptional (Just (ReprText "bar")))
    :& RNil
