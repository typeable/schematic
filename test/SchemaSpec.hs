module SchemaSpec (spec, main) where

import Control.Monad
import Data.Aeson
import Data.Proxy
import Data.Schematic
import Data.Singletons
import Data.Vinyl
import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series.Instances


type SchemaExample
  = SchemaObject
    '[ '("foo", SchemaArray '[AEq 1] (SchemaNumber '[NGt 10]))
     , '("bar", SchemaText '[TRegex "\\w+"])]

exampleTest :: JsonRepr (SchemaText '[TEq 3])
exampleTest = ReprText "lil"

exampleNumber :: JsonRepr (SchemaNumber '[NGt 10])
exampleNumber = ReprNumber 12

exampleArray :: JsonRepr (SchemaArray '[AEq 1] (SchemaNumber '[NGt 10]))
exampleArray = ReprArray [exampleNumber]

exampleObject :: JsonRepr SchemaExample
exampleObject = ReprObject $ FieldRepr exampleArray :& FieldRepr (ReprText "barval") :& RNil

jsonExample :: JsonRepr SchemaExample
jsonExample = ReprObject $
  FieldRepr (ReprArray [ReprNumber 12])
    :& FieldRepr (ReprText "tes")
    :& RNil

-- schemaExample :: Sing SchemaExample
-- schemaExample = known

spec :: Spec
spec = do
  -- it "show/read JsonRepr properly" $
  --   read (show example) == example
  it "decode/encode JsonRepr properly" $
    decode (encode jsonExample) == Just jsonExample

main :: IO ()
main = hspec spec
