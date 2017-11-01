# schematic

[![Build Status](https://travis-ci.org/typeable/schematic.svg?branch=master)](https://travis-ci.org/typeable/schematic)

## Goal

The goal of the library is to provide a type-safe transport layer for serializing and validating JSON. It can be thought of as a subset of [json-schema](http://json-schema.org), which is basically a specification of a JSON document. The other goal is getting as much as possible from this specification for free. Right now the following bits are prototyped:

* All of the instantiations of the transport types structurally follow the schema provided by user
* Serializers are generic, so they follow from the type-level schema and the're supposed to have a roundtrip property by an implementation.
* Runtime value validators are generated from the schema. Validation errors are reported as a pairs of a json-path to the element and an error message.
* There are migrations. It's possible to describe a series of migrations to the schema and have all the necessary machinery to deserialize a user specified version of the schema if there are multiple versions available.
* Schematic schemas can be exported to [json-schema](http://json-schema.org)

Be aware that library is experimental and subject to change a lot. The current state can be viewed as a prototype.

## Installation

* Install [Stack](https://github.com/commercialhaskell/stack)

```
$ stack install schematic
```

## GHC Extensions

To use this library without any hassle, you should add a few GHC extension
either to a module or a cabal file:

```
DataKinds
OverloadedLists
OverloadedStrings
TypeApplications
```

`Overloaded`-extensions are being used only by `field` combinator,
so it you don't use it - feel free to disable it.

## GHC Options

I recommend using `{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}`
pragma in modules which declare schemas and schema migrations, because `schematic`
heavily uses promoted types which should be prefixed with "`" signs and they can
be dropped with this option, making it less noisy visually.
It's entirely optional and totally opinionated, so feel free to ignore this.
Through the documentation, the code is written without any explicit ticks for
the same reason.
Notice that they're still needed for type-level lists and tuples in schematic code.

## Basic Examples

```haskell
type SchemaExample
  = SchemaObject
    '[ '("foo", SchemaArray '[AEq 1] (SchemaNumber '[NGt 10]))
     , '("bar", SchemaOptional (SchemaText '[TEnum '["foo", "bar"]]))]
```

* This schema says that's structurally it's a JSON object which has two fields:
  * field "foo" contains an array of numeric values
  * field "bar" contains an optional text value
* It also allows you to validate JSON values itself:
  * an array in "foo" contains exactly one element
  * that element must be strictly greater than 10
  * value of "bar" field must be either "foo" or "bar" if presented


This one is valid for example

```haskell
schemaJson :: ByteString
schemaJson = "{\"foo\": [13], \"bar\": null}"
```

Also valid

```haskell
schemaJson :: ByteString
schemaJson = "{\"foo\": [13], \"bar\": \"bar\"}"
```

it can be parsed and validated like this:

```haskell
decodeAndValidateJson schemaJson :: ParseResult (JsonRepr SchemaExample)
```

`ParseResult` type encodes three possible situations:
* json is structurally consistent with a schema and runtime validations pass
* json source is malformed or doesn't correspond to a schema structurally
* some of the runtime validations have failed, but it's structurally correct
  overall

It implies a transport layer representation of that data that can be traversed
and transformed to whatever internal types user has.
It's called `JsonRepr`. Type parameter is the schema itself.

```haskell
jsonExample :: JsonRepr SchemaExample
jsonExample = withRepr @SchemaExample
   $  field @"foo" [12]
   :& field @"bar" (Just "bar")
   :& RNil
```

The most basic and sugar-free way of constructing the same object will look like this:

```
jsonExample' :: JsonRepr SchemaExample
jsonExample' = ReprObject $
  FieldRepr (ReprArray [ReprNumber 12])
    :& FieldRepr (ReprOptional (Just (ReprText "bar")))
    :& RNil
```

## Lens-compatibility

It's possible to use `flens` to construct a field lens for a typed object in schematic. Let's suppose you have a schema like this:

```haskell
type ArraySchema = SchemaArray '[AEq 1] (SchemaNumber '[NGt 10])

type ArrayField = '("foo", ArraySchema)

type FieldsSchema =
  '[ ArrayField, '("bar", SchemaOptional (SchemaText '[TEnum '["foo", "bar"]]))]

type SchemaExample = 'SchemaObject FieldsSchema

```

There are two ways of working with named fields in the objects:

* Using the `fget` and `fset` functions: `fget @"foo" (fput newFooVal objectData) == newFooVal`
* Using the lens library: `set (flens @"foo") newFooVal objectData ^. flens @"foo" == newFooVal`

## Migrations

Usually migrations are implicit and hard to deal with, `schematic` deals with it
by giving tools to deal with it to a programmer, being as explicit as possible,
notifying the developer of a missing transition with a compile-time error.
It allows to build versioned HTTP APIs or migrate data from the older versions
when reading from the JSON storages.

It's possible to represent schema changes as a series of migrations,
which describes a series of json-path/change pairs. Migrations can be applied in
succession.

This piece of code will apply a migration to the schema, decode and validate the latest version.

```haskell
type SchemaExample
  = SchemaObject
    '[ '("foo", SchemaArray '[AEq 1] (SchemaNumber '[NGt 10]))
     , '("bar", SchemaOptional (SchemaText '[TEnum '["foo", "bar"]]))]

type TestMigration =
  'Migration "test_revision"
    '[ Diff '[ PKey "bar" ] (Update (SchemaText '[]))
     , Diff '[ PKey "foo" ] (Update (SchemaNumber '[])) ]

type VS = 'Versioned SchemaExample '[ TestMigration ]

schemaJsonTopVersion :: ByteString
schemaJsonTopVersion = "{ \"foo\": 42, \"bar\": \"bar\" }"
```

It's possible to decode the latest version like this:

```
decodeAndValidateJson schemaJsonTopVersion :: ParseResult (JsonRepr (TopVersion (AllVersions VS)))
```

It's important to differentiate between schema migrations and data migrations.
Schema migration is a change of a schema acceptable by a JSON consumer. It transforms one schema to another.
Data migrations on the other hand are functions on the data itself. They transform values
acceptable by one schema to values acceptable by another.
Let's look at the situation when we have to add a field to a JSON Object of a current schema:

```haskell
type SchemaExample = 'SchemaObject
  '[ '("foo", 'SchemaArray '[ 'AEq 1] ('SchemaNumber '[ 'NGt 10]))
   , '("bar", 'SchemaOptional ('SchemaText '[ 'TEnum '["foo", "bar"]]))]

jsonExample :: JsonRepr SchemaExample
jsonExample = withRepr @SchemaExample
   $ field @"bar" (Just "bar")
  :& field @"foo" [12]
  :& RNil

type AddQuuz =
  'Migration "add_field_quuz"
   '[ 'Diff '[] ('AddKey "quuz" (SchemaNumber '[])) ]

type DeleteQuuz =
  'Migration "remove_field_quuz"
    '[ 'Diff '[] ( 'DeleteKey "quuz") ]

type Migrations = '[ AddQuuz
                   , DeleteQuuz ]

type VersionedJson = 'Versioned SchemaExample Migrations

migrationList :: MigrationList Identity VersionedJson
migrationList
  =   (migrateObject (\r -> Identity $ field @"quuz" 42 :& r))
  :&& shrinkObject
  :&& MNil

```

In this instance `Migrations` is a list of schema migrations and
`MigrationList Identity VersionedJson` is a list of data migrations corresponding
to the list of schema migrations: `migrateObject (\r -> Identity $ field @"quuz" 42 :& r))`
will be used at runtime to transform `JsonRepr SchemaExample` to
`JsonRepr (SchemaByRevision "add_quuz" VersionedJson). `Identity` in the type of
`MigrationList` is a monad we choose to run our migrations in. If there's need to
do database queries or something like that, it can be changed to something more
appropriate by a user.
`migrateObject` takes a function transforming old fields into new ones, `shrinkObject`
allows to shrink the JSON object in case migration only removed fields.

## How do I construct a value of `JsonRepr schema`?

`JsonRepr schema` is a primary representation of a value serializable/deserializable
to JSON, but with a twist. It's guaranteed to correspond a type-level schema,
it's a type parameter of a same name.
It makes constructing it a little bit more involved, but luckily `schematic`
provides a DSL for doing so in a more straightforward fashion. An example would
look like this:

```haskell
type SchemaExample = 'SchemaObject
  '[ '("foo", SchemaArray '[ AEq 1] (SchemaNumber '[NGt 10]))
   , '("bar", SchemaOptional (SchemaText '[TEnum '["foo", "bar"]]))]

jsonExample = withRepr @SchemaExample
   $  field @"bar" (Just "bar")
   :& field @"foo" [12]
   :& RNil
```

`@foo` syntax is an explicit type application, which is a feature of GHC 8+, it
makes type inference possible without relying on a bunch of `Proxy`s, which
makes it syntactically more terse.

[GHC Type Applications](https://ghc.haskell.org/trac/ghc/wiki/TypeApplication)

`schematic` provides instances for `OverloadedLists` and `OverloadedStrings`
GHC extensions to make use of string and list literals for values in a previous
example.


## Export to json-schema (draft 4)
