module SchemaSpec (spec, main) where

import Data.Proxy
import Data.Schematic.Schema
import Test.Hspec

type TextSchema = SText '[LengthEqNat 3]

type NumberSchema = SNumber '[ Gt 3]

type ObjectSchema = SObject '[ Field "carrier" 'JText [TextSchema] ]

spec :: Spec
spec = do
  describe "Schema translation: " $ do
    let
      textSchema   = SchemaText [LengthEq 2]
      numberSchema = SchemaNumber [Gt 3]
    it "translate string schema"
      $ build (Proxy @TextSchema) `shouldBe` textSchema
    it "translates number schema"
      $ build (Proxy @NumberSchema) `shouldBe` numberSchema
    it "translates object schema" $ do
      build (Proxy @ObjectSchema) `shouldBe`
        SchemaObject [("carrier", textSchema), ("number_gt_3", numberSchema)]

main :: IO ()
main = hspec spec
