{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.SwaggerSpec where

import Data.Aeson
import Data.Aeson.QQ
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import qualified Data.Vector as Vector

import Data.Swagger

import Test.Hspec
import Test.Hspec.HUnit

isSubJSON :: Value -> Value -> Bool
isSubJSON Null _ = True
isSubJSON (Object x) (Object y) = HashMap.keys x == HashMap.keys i && and i
  where
    i = HashMap.intersectionWith isSubJSON x y
isSubJSON (Array xs) (Array ys) = Vector.length xs == Vector.length ys && and (Vector.zipWith isSubJSON xs ys)
isSubJSON x y = x == y

(<~>) :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Value -> Spec
x <~> json = do
  it "encodes correctly (probably with extra properties)" $ do
    toJSON x `shouldSatisfy` (json `isSubJSON`)
  it "decodes correctly" $ do
    fromJSON json `shouldBe` Success x

(<=>) :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Value -> Spec
x <=> json = do
  it "encodes correctly" $ do
    toJSON x `shouldBe` json
  it "decodes correctly" $ do
    fromJSON json `shouldBe` Success x

spec :: Spec
spec = do
  describe "Operation Object" $ operationExample <~> operationExampleJSON
  describe "License Object" $ licenseExample <=> licenseExampleJSON
  describe "Contact Object" $ contactExample <=> contactExampleJSON
  describe "Info Object" $ infoExample <=> infoExampleJSON

main :: IO ()
main = hspec spec

-- =======================================================================
-- Info object
-- =======================================================================

infoExample :: SwaggerInfo
infoExample = SwaggerInfo
  { swaggerInfoTitle = "Swagger Sample App"
  , swaggerInfoDescription = Just "This is a sample server Petstore server."
  , swaggerInfoTermsOfService = Just "http://swagger.io/terms/"
  , swaggerInfoContact = Just contactExample
  , swaggerInfoLicense = Just licenseExample
  , swaggerInfoVersion = "1.0.1" }
  where
    license = SwaggerLicense
      { swaggerLicenseName = "Apache 2.0"
      , swaggerLicenseUrl = Just (URL "http://www.apache.org/licenses/LICENSE-2.0.html") }

infoExampleJSON :: Value
infoExampleJSON = [aesonQQ|
{
  "title": "Swagger Sample App",
  "description": "This is a sample server Petstore server.",
  "termsOfService": "http://swagger.io/terms/",
  "contact": {
    "name": "API Support",
    "url": "http://www.swagger.io/support",
    "email": "support@swagger.io"
  },
  "license": {
    "name": "Apache 2.0",
    "url": "http://www.apache.org/licenses/LICENSE-2.0.html"
  },
  "version": "1.0.1"
}
|]

-- =======================================================================
-- Contact object
-- =======================================================================

contactExample :: SwaggerContact
contactExample = SwaggerContact
  { swaggerContactName = Just "API Support"
  , swaggerContactUrl = Just (URL "http://www.swagger.io/support")
  , swaggerContactEmail = Just "support@swagger.io" }

contactExampleJSON :: Value
contactExampleJSON = [aesonQQ|
{
  "name": "API Support",
  "url": "http://www.swagger.io/support",
  "email": "support@swagger.io"
}
|]

-- =======================================================================
-- License object
-- =======================================================================

licenseExample :: SwaggerLicense
licenseExample = SwaggerLicense
  { swaggerLicenseName = "Apache 2.0"
  , swaggerLicenseUrl = Just (URL "http://www.apache.org/licenses/LICENSE-2.0.html") }

licenseExampleJSON :: Value
licenseExampleJSON = [aesonQQ|
{
  "name": "Apache 2.0",
  "url": "http://www.apache.org/licenses/LICENSE-2.0.html"
}
|]


-- =======================================================================
-- Operation object
-- =======================================================================

operationExample :: SwaggerOperation
operationExample = SwaggerOperation
  { swaggerOperationTags = ["pet"]
  , swaggerOperationSummary = Just "Updates a pet in the store with form data"
  , swaggerOperationDescription = Just ""
  , swaggerOperationOperationId = Just "updatePetWithForm"
  , swaggerOperationConsumes = Just (SwaggerMimeList ["application/x-www-form-urlencoded"])
  , swaggerOperationProduces = Just (SwaggerMimeList ["application/json", "application/xml"])
  , swaggerOperationParameters = params
  , swaggerOperationResponses = responses
  , swaggerOperationSecurity = security
  , swaggerOperationExternalDocs = Nothing
  , swaggerOperationSchemes = Nothing
  , swaggerOperationDeprecated = False
  }
  where
    security = [SwaggerSecurityRequirement [("petstore_auth", ["write:pets", "read:pets"])]]

    responses = SwaggerResponses
      { swaggerResponsesDefault = Nothing
      , swaggerResponsesResponses =
          [ (200, SwaggerResponse
                    { swaggerResponseDescription = "Pet updated."
                    , swaggerResponseSchema = Nothing
                    , swaggerResponseHeaders = mempty
                    , swaggerResponseExamples = Nothing } )
          , (405, SwaggerResponse
                    { swaggerResponseDescription = "Invalid input"
                    , swaggerResponseSchema = Nothing
                    , swaggerResponseHeaders = mempty
                    , swaggerResponseExamples = Nothing } ) ] }

    params =
      [ SwaggerParameter
          { swaggerParameterName = "petId"
          , swaggerParameterDescription = Just "ID of pet that needs to be updated"
          , swaggerParameterRequired = True
          , swaggerParameterSchema = SwaggerParameterOther (stringSchema SwaggerParameterPath) }
      , SwaggerParameter
          { swaggerParameterName = "name"
          , swaggerParameterDescription = Just "Updated name of the pet"
          , swaggerParameterRequired = False
          , swaggerParameterSchema = SwaggerParameterOther (stringSchema SwaggerParameterFormData) }
      , SwaggerParameter
          { swaggerParameterName = "status"
          , swaggerParameterDescription = Just "Updated status of the pet"
          , swaggerParameterRequired = False
          , swaggerParameterSchema = SwaggerParameterOther (stringSchema SwaggerParameterFormData) }
      ]

    stringSchema i = SwaggerParameterOtherSchema
      { swaggerParameterOtherSchemaIn = i
      , swaggerParameterOtherSchemaType = SwaggerParamString
      , swaggerParameterOtherSchemaFormat = Nothing
      , swaggerParameterOtherSchemaAllowEmptyValue = False
      , swaggerParameterOtherSchemaItems = Nothing
      , swaggerParameterOtherSchemaCollectionFormat = Nothing
      , swaggerParameterOtherSchemaCommon = emptySchemaCommon
      }

    emptySchemaCommon = SwaggerSchemaCommon
      { swaggerSchemaDefault = Nothing
      , swaggerSchemaMaximum = Nothing
      , swaggerSchemaExclusiveMaximum = Nothing
      , swaggerSchemaMinimum = Nothing
      , swaggerSchemaExclusiveMinimum = Nothing
      , swaggerSchemaMaxLength = Nothing
      , swaggerSchemaMinLength = Nothing
      , swaggerSchemaPattern = Nothing
      , swaggerSchemaMaxItems = Nothing
      , swaggerSchemaMinItems = Nothing
      , swaggerSchemaUniqueItems = Nothing
      , swaggerSchemaEnum = Nothing
      , swaggerSchemaMultipleOf = Nothing }

operationExampleJSON :: Value
operationExampleJSON = [aesonQQ|
{
  "tags": [
    "pet"
  ],
  "summary": "Updates a pet in the store with form data",
  "description": "",
  "operationId": "updatePetWithForm",
  "consumes": [
    "application/x-www-form-urlencoded"
  ],
  "produces": [
    "application/json",
    "application/xml"
  ],
  "parameters": [
    {
      "name": "petId",
      "in": "path",
      "description": "ID of pet that needs to be updated",
      "required": true,
      "type": "string"
    },
    {
      "name": "name",
      "in": "formData",
      "description": "Updated name of the pet",
      "required": false,
      "type": "string"
    },
    {
      "name": "status",
      "in": "formData",
      "description": "Updated status of the pet",
      "required": false,
      "type": "string"
    }
  ],
  "responses": {
    "200": {
      "description": "Pet updated."
    },
    "405": {
      "description": "Invalid input"
    }
  },
  "security": [
    {
      "petstore_auth": [
        "write:pets",
        "read:pets"
      ]
    }
  ]
}
|]
