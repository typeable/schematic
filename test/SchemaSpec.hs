module SchemaSpec (spec, main) where

import Data.Functor.Foldable
import Data.Proxy
import Data.Schematic.Schema
import Data.Schematic.Validation
import Test.Hspec


type TextSchema = SText '[LengthEqNat 2]

type NumberSchema = SNumber '[ Gt 3]

type ArraySchema = SArray '[ LengthEqNat 3 ] NumberSchema

type ObjectSchema
  = SObject
    '[ Field "carrier" 'JText '[LengthEqNat 2]
     , Field "number_gt_3" 'JNumber '[Gt 3] ]

type GeneratorSchema
  = SObject
    '[ Generate "Carrier" (Field "carrier" 'JText '[LengthEqNat 2])
     , Field "number_gt_3" 'JNumber '[Gt 3] ]

type BiggerSchema
  = Generate "Object"
    ( SObject
      '[ Generate "Carrier" (Field "carrier" 'JText '[LengthEqNat 2])
       , Field "number_gt_3" 'JNumber '[Gt 3] ])

spec :: Spec
spec = do
  let
    textSchema   = Fix $ SchemaText SJText [LengthEq 2]
    numberSchema = Fix $ SchemaNumber SJNumber [Gt 3]
    arraySchema  = Fix $ SchemaArray SJArray [LengthArrEq 3]
      $ Fix $ SchemaNumber SJNumber [Gt 3]
    objectSchema = Fix $ SchemaObject SJObject
      [ ("carrier", textSchema)
      , ("number_gt_3", numberSchema)]

  describe "Schema translation: " $ do
    it "translate string schema"
      $ build (Proxy @TextSchema) `shouldBe` textSchema
    it "translates number schema"
      $ build (Proxy @NumberSchema) `shouldBe` numberSchema
    it "translates array schema" $ do
      build (Proxy @ArraySchema) `shouldBe` arraySchema
    it "translates object schema" $ do
      build (Proxy @ObjectSchema) `shouldBe` objectSchema
    it "translates Generate symbols" $ do
      build (Proxy @GeneratorSchema) `shouldBe` objectSchema

  describe "Types listing: " $ do
    it "lists types"
      $ typesOf (build (Proxy @BiggerSchema))
        `shouldBe` [("Carrier", textSchema), ("Object", objectSchema)]

main :: IO ()
main = hspec spec
