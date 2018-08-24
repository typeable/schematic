{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Schematic.Migration where

import Data.Kind
import Data.List
import Data.Schematic.DSL
import Data.Schematic.Lens
import Data.Schematic.Path
import Data.Schematic.Schema
import Data.Singletons.Prelude hiding (All, (:.))
import Data.Singletons.TypeLits
import Data.Tagged
import Data.Vinyl
import Data.Vinyl.Functor


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
  DeleteKey acc fn ('(fn, a) ': tl) = acc ++ tl
  DeleteKey acc fn (fna ': tl) = acc ++ (fna ': tl)

type family UpdateKey
  (fn :: Symbol)
  (fs :: [(Symbol, Schema)])
  (s :: Schema) = (r :: [(Symbol, Schema)]) where
  UpdateKey fn ( '(fn, a) ': tl ) s = ( '(fn, s) ': tl )
  UpdateKey fn ( '(fs, a) ': tl ) s = ( '(fs, a) ': UpdateKey fn tl s)

-- schema updates

type family Build (b :: Builder) :: Schema where
  Build ('BKey ('SchemaObject fs) fn z) = 'SchemaObject (UpdateKey fn fs (Build z))
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

type family InitialSchema (v :: Versioned) = (s :: Schema) where
  InitialSchema ('Versioned s ms) = s

type family ElemOf (e :: k) (l :: [(a,k)]) :: Constraint where
  ElemOf e '[] = 'True ~ 'False
  ElemOf e ( '(a, e) ': es) = ()
  ElemOf e (n ': es) = ElemOf e es

-- | Extracts revision/schema pairs from @Versioned@ in reverse order.
type family AllVersions (vd :: Versioned) :: [(Revision, Schema)] where
  AllVersions ('Versioned s ms) = Reverse (AllVersions' '[ '("initial", s) ] ms)

type family AllVersions' (acc :: [(Revision, Schema)]) (ms :: [Migration])
  = (r :: [(Revision, Schema)]) where
  AllVersions' acc '[]                       = acc
  AllVersions' ( '(rh, sh) ': tl ) (m ': ms) =
    AllVersions' ( '(rh, sh) ': ApplyMigration m sh ': tl ) ms

type family TopVersion (rs :: [(Revision, Schema)]) :: Schema where
  TopVersion ( '(rh, sh) ': tl) = sh

data Action = AddKey Symbol Schema | Update Schema | DeleteKey Symbol

data instance Sing (a :: Action) where
  SAddKey
    :: Sing n
    -> Sing s
    -> Sing ('AddKey n s)
  SUpdate :: Sing s -> Sing ('Update s)
  SDeleteKey :: Sing s -> Sing ('DeleteKey s)

-- | User-supplied atomic difference between schemas.
-- Migrations can consists of many differences.
data Diff = Diff [Path] Action

data instance Sing (diff :: Diff) where
  SDiff
    :: Sing (jp :: [Path])
    -> Sing (a :: Action)
    -> Sing ('Diff jp a)

-- | User-provided name of the revision.
type Revision = Symbol

data Migration = Migration Revision [Diff]

data instance Sing (m :: Migration) where
  SMigration
    :: Sing r
    -> Sing ds
    -> Sing ('Migration r ds)

data Versioned = Versioned Schema [Migration]

data instance Sing (v :: Versioned) where
  SVersioned
    :: Sing (s :: Schema)  -- base version
    -> Sing (ms :: [Migration]) -- a bunch of migrations
    -> Sing ('Versioned s ms)

type DataMigration s m h = Tagged s (JsonRepr h -> m (JsonRepr s))

data MList :: (Type -> Type) -> [Schema] -> Type where
  MNil :: (Monad m, SingI s, TopLevel s) => MList m '[s]
  (:&&)
    :: (TopLevel s, SingI s)
    => DataMigration s m h
    -> MList m (h ': tl)
    -> MList m (s ': h ': tl)

infixr 7 :&&

migrateObject
  :: forall m fs fh. (FSubset fs fs (FImage fs fs), Monad m)
  => (Rec (Tagged fs :. FieldRepr) fh -> m (Rec (Tagged fs :. FieldRepr) fs))
  -> Tagged ('SchemaObject fs) (JsonRepr ('SchemaObject fh) -> m (JsonRepr ('SchemaObject fs)))
migrateObject f = Tagged $ \(ReprObject r) -> do
  res <- f $ rmap (Compose . Tagged) r
  pure $ withRepr @('SchemaObject fs) res

shrinkObject
  :: forall rs ss m
  . ( Monad m, FSubset rs ss (FImage rs ss) )
  => Tagged
    ('SchemaObject rs)
    (JsonRepr ('SchemaObject ss) -> m (JsonRepr ('SchemaObject rs)))
shrinkObject = Tagged $ \(ReprObject r) -> pure $ ReprObject $ fcast r

type family MapSnd (l :: [(a,k)]) = (r :: [k]) where
  MapSnd '[] = '[]
  MapSnd ( '(a, b) ': tl) = b ': MapSnd tl

type MigrationList m vs = MList m (MapSnd (AllVersions vs))
