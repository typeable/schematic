module SchemaSpec (spec, main) where

import Data.Aeson
import Data.Proxy
import Data.Schematic
import Data.Singletons
import Data.Vinyl
import Test.Hspec
import Test.Hspec.SmallCheck


type SchemaExample
  = SchemaObject
    '[ '("foo", SchemaArray '[AEq 3] (SchemaNumber '[NGt 10]))
     , '("bar", SchemaText '[Regex "\\w+"])]

schemaExample :: (SingKind Schema, SingI SchemaExample) => DemoteRep Schema
schemaExample = fromSing (sing :: Sing SchemaExample)

exampleTest :: JsonRepr (SchemaText '[TEq 3])
exampleTest = ReprText "lilo"

exampleNumber :: JsonRepr (SchemaNumber '[NEq 3])
exampleNumber = ReprNumber 3

exampleArray :: JsonRepr (SchemaArray '[AEq 1] (SchemaNumber '[NEq 3]))
exampleArray = ReprArray [exampleNumber]

exampleObject
  :: JsonRepr (SchemaObject '[ '("foo", SchemaArray '[AEq 1] (SchemaNumber '[NEq 3]))] )
exampleObject = ReprObject $ FieldRepr exampleArray :& RNil

example :: JsonRepr SchemaExample
example = ReprObject $
  FieldRepr (ReprArray [ReprNumber 3])
    :& FieldRepr (ReprText "test")
    :& RNil

spec :: Spec
spec = do
  it "decode/encode JsonRepr properly" $ do
    property $ \(a :: JsonRepr SchemaExample) -> decode (encode a) == Just a

main :: IO ()
main = hspec spec
