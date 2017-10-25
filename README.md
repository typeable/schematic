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

## Basic Examples

```
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

```
schemaJson :: ByteString
schemaJson = "{\"foo\": [13], \"bar\": null}"
```

Also valid

```
schemaJson :: ByteString
schemaJson = "{\"foo\": [13], \"bar\": \"bar\"}"
```

it can be parsed and validated like this:

```
decodeAndValidateJson schemaJson :: ParseResult (JsonRepr SchemaExample)
```

`ParseResult` type encodes three possible situations:
* json is structurally consistent with a schema and runtime validations pass
* json source is malformed or doesn't correspond to a schema structurally
* some of the runtime validations have failed, but it's structurally correct
  overall

It implies a transport layer representation of that data that can be traversed and transformed to whatever internal types user has. It's called `JsonRepr`. Type parameter is the schema itself.


```
jsonExample :: JsonRepr SchemaExample
jsonExample = ReprObject $
  FieldRepr (ReprArray [ReprNumber 12])
    :& FieldRepr (ReprOptional (Just (ReprText "bar")))
    :& RNil
```

## Migrations

It's possible to represent schema changes as a series of migrations, which describes a series of json-path/change pairs. Migrations can be applied in succession.

This piece of code will apply a migration to the schema, decode and validate the latest version.

```
type SchemaExample
  = SchemaObject
    '[ '("foo", SchemaArray '[AEq 1] (SchemaNumber '[NGt 10]))
     , '("bar", SchemaOptional (SchemaText '[TEnum '["foo", "bar"]]))]

type TestMigration =
  'Migration "test_revision"
    '[ 'Diff '[ 'PKey "bar" ] ('Update ('SchemaText '[]))
     , 'Diff '[ 'PKey "foo" ] ('Update ('SchemaNumber '[])) ]

type VS = 'Versioned SchemaExample '[ TestMigration ]

schemaJsonTopVersion :: ByteString
schemaJsonTopVersion = "{ \"foo\": 42, \"bar\": \"bar\" }"
```

It's possible to decode the latest version like this:

```
decodeAndValidateJson schemaJsonTopVersion :: ParseResult (JsonRepr (TopVersion (AllVersions VS)))
```

## Lens-compatibility

It's possible to use `flens` to construct a field lens for a typed object in schematic. Let's suppose you have a schema like this:

```
type ArraySchema = 'SchemaArray '[AEq 1] ('SchemaNumber '[NGt 10])

type ArrayField = '("foo", ArraySchema)

type FieldsSchema =
  '[ ArrayField, '("bar", 'SchemaOptional ('SchemaText '[TEnum '["foo", "bar"]]))]

type SchemaExample = 'SchemaObject FieldsSchema

```

There are two ways of working with named fields in the objects:

* Using the `fget` and `fset` functions: `fget fooProxy (fput newFooVal objectData) == newFooVal`
* Using the lens library: `set (flens (Proxy @"foo")) newFooVal objectData ^. flens (Proxy @"foo") == newFooVal`

## Export to json-schema (draft 4)
