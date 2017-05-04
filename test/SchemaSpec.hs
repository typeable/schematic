module SchemaSpec (spec, main) where

import Data.Proxy
import Data.Schematic.Schema
import Test.Hspec

type TextSchema = SText '[LengthEqNat 2]

type NumberSchema = SNumber '[ Gt 3]

type ArraySchema = SArray '[ LengthEqNat 3 ] NumberSchema

type ObjectSchema
  = SObject '[ Field "carrier" 'JText '[LengthEqNat 2], Field "number_gt_3" 'JNumber '[Gt 3] ]

spec :: Spec
spec = do
  describe "Schema translation: " $ do
    let
      textSchema   = SchemaText [LengthEq 2]
      numberSchema = SchemaNumber [Gt 3]
      arraySchema  = SchemaArray [LengthArrEq 3] $ SchemaNumber [Gt 3]
    it "translate string schema"
      $ build (Proxy @TextSchema) `shouldBe` textSchema
    it "translates number schema"
      $ build (Proxy @NumberSchema) `shouldBe` numberSchema
    it "translates array schema" $ do
      build (Proxy @ArraySchema) `shouldBe` arraySchema
    it "translates object schema" $ do
      build (Proxy @ObjectSchema) `shouldBe`
        SchemaObject [("carrier", textSchema), ("number_gt_3", numberSchema)]

main :: IO ()
main = hspec spec
