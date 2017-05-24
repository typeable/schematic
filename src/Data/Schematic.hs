module Data.Schematic
  ( module Data.Schematic.Schema
  , module Data.Schematic.Utils
  , decodeAndValidateJson
  , parseAndValidateJson
  , isValid
  , isDecodingError
  , isValidationError
  , ParseResult(..)
  ) where

import Control.Monad.Validation
import Data.Aeson as J
import Data.Aeson.Types as J
import Data.ByteString.Lazy as BL
import Data.Functor.Identity
import Data.Schematic.Schema
import Data.Schematic.Utils
import Data.Schematic.Validation
import Data.Text as T


parseAndValidateJson
  :: forall schema
  .  (J.FromJSON (JsonRepr schema), TopLevel schema, Known (Sing schema))
  => J.Value
  -> ParseResult (JsonRepr schema)
parseAndValidateJson v =
  case parseEither parseJSON v of
    Left s         -> DecodingError $ T.pack s
    Right jsonRepr ->
      let
        validate = validateJsonRepr (known :: Sing schema) [] jsonRepr
        res      = runIdentity . runValidationTEither $ validate
      in case res of
        Left em  -> ValidationError em
        Right () -> Valid jsonRepr

decodeAndValidateJson
  :: forall schema
  .  (J.FromJSON (JsonRepr schema), TopLevel schema, Known (Sing schema))
  => BL.ByteString
  -> ParseResult (JsonRepr schema)
decodeAndValidateJson bs = case decode bs of
  Nothing -> DecodingError "invalid json"
  Just x  -> parseAndValidateJson x
