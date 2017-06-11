# schematic

[![Build Status](https://travis-ci.org/dredozubov/schematic.svg?branch=master)](https://travis-ci.org/dredozubov/schematic)

## Goal

The goal of the library is to provide a type-safe transport layer for serializing and validating JSON. It can be thought of as a subset of [json-schema](http://json-schema.org), which is basically a specification of a JSON document. The other goal is getting as much as possible from this specification for free. Right now the following bits are prototyped:

* All the instantiations of transport types are structurally follow the schema provided by user
* Serializers are generic, so they follow from the type-level schema and the're supposed to have roundtrip property by implementation.
* Runtime value validators are generated from the schema. Validation errors are reported as a pairs of a json-path to the element and an error message.
* There are migrations. It's possible to describe a series of migrations to the schema and have all the machinery deserialize a user specified version of the schema if there's a few versions.

## TODO

* Making a migration system more usable. It should allow it to be used as a json safecopy solution minus the boilerplate.
* Describing versioned http endpoints
* Schema pretty-printing
* More validators
* Overall user friendliness

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

* This schema says that's structurally it's a JSON object which has two field:
  * field "foo" contains an of numeric values
  * field "bar" contains an optional textual value
* It also allows you to validate the JSON:
  * an array in "foo" must contain exactly one element
  * that element must be strictly greater than 10
  * value of bar must be either "foo" or "bar" if presented


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
* json is structurally consistent with schema and runtime validations passed
* json source is malformed or doesn't correspond to schema structurally
* some of the runtime validations failed, but it's structurally correct

I t's build a transport layer representation of that data that can be tranversed and transformed to whatever internal types user has. Transport layer represetation looks like this. Type parameter is the schema itself.


```
jsonExample :: JsonRepr SchemaExample
jsonExample = ReprObject $
  FieldRepr (ReprArray [ReprNumber 12])
    :& FieldRepr (ReprOptional (Just (ReprText "bar")))
    :& RNil
```

## Migrations

It's possible to represent schema changes as a migration, which describes a series of json-path/change pairs. Migrations can be applied in succession.

This piece of code will apply a migration to the schema, then decodes and validates the latest version.

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
