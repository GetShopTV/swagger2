{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.SwaggerSpec where

import Data.Aeson
import Data.Aeson.QQ
import qualified Data.Foldable as F
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Monoid
import qualified Data.Vector as Vector
import Data.Text (Text)

import Data.Swagger

import Test.Hspec

isSubJSON :: Value -> Value -> Bool
isSubJSON Null _ = True
isSubJSON (Object x) (Object y) = HashMap.keys x == HashMap.keys i && F.and i
  where
    i = HashMap.intersectionWith isSubJSON x y
isSubJSON (Array xs) (Array ys) = Vector.length xs == Vector.length ys && F.and (Vector.zipWith isSubJSON xs ys)
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
  describe "License Object" $ licenseExample <=> licenseExampleJSON
  describe "Contact Object" $ contactExample <=> contactExampleJSON
  describe "Info Object" $ infoExample <=> infoExampleJSON
  describe "Operation Object" $ operationExample <~> operationExampleJSON
  describe "Schema Object" $ do
    context "Primitive Sample" $ schemaPrimitiveExample <~> schemaPrimitiveExampleJSON
    context "Simple Model" $ schemaSimpleModelExample <~> schemaSimpleModelExampleJSON
    context "Model with Map/Dictionary Properties" $ schemaModelDictExample <~> schemaModelDictExampleJSON
    context "Model with Example" $ schemaWithExampleExample <~> schemaWithExampleExampleJSON
  describe "Definitions Object" $ definitionsExample <~> definitionsExampleJSON
  describe "Parameters Definition Object" $ parametersDefinitionExample <~> parametersDefinitionExampleJSON
  describe "Responses Definition Object" $ responsesDefinitionExample <~> responsesDefinitionExampleJSON
  describe "Security Definitions Object" $ securityDefinitionsExample <~> securityDefinitionsExampleJSON
  describe "Swagger Object" $ swaggerExample <~> swaggerExampleJSON

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
operationExample = mempty
  { swaggerOperationTags = ["pet"]
  , swaggerOperationSummary = Just "Updates a pet in the store with form data"
  , swaggerOperationDescription = Just ""
  , swaggerOperationOperationId = Just "updatePetWithForm"
  , swaggerOperationConsumes = Just (SwaggerMimeList ["application/x-www-form-urlencoded"])
  , swaggerOperationProduces = Just (SwaggerMimeList ["application/json", "application/xml"])
  , swaggerOperationParameters = params
  , swaggerOperationResponses = responses
  , swaggerOperationSecurity = security
  }
  where
    security = [SwaggerSecurityRequirement [("petstore_auth", ["write:pets", "read:pets"])]]

    responses = mempty
      { swaggerResponsesResponses =
          [ (200, SwaggerInline mempty { swaggerResponseDescription = "Pet updated." })
          , (405, SwaggerInline mempty { swaggerResponseDescription = "Invalid input" }) ] }

    params = map SwaggerInline
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

    stringSchema i = mempty
      { swaggerParameterOtherSchemaIn = i
      , swaggerParameterOtherSchemaType = SwaggerParamString
      }

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

-- =======================================================================
-- Schema object
-- =======================================================================

schemaPrimitiveExample :: SwaggerSchema
schemaPrimitiveExample = mempty
  { swaggerSchemaType = SwaggerSchemaString
  , swaggerSchemaFormat = Just "email"
  }

schemaPrimitiveExampleJSON :: Value
schemaPrimitiveExampleJSON = [aesonQQ|
{
    "type": "string",
    "format": "email"
}
|]

schemaSimpleModelExample :: SwaggerSchema
schemaSimpleModelExample = mempty
  { swaggerSchemaType = SwaggerSchemaObject
  , swaggerSchemaRequired = [ "name" ]
  , swaggerSchemaProperties =
      [ ("name", SwaggerInline mempty
            { swaggerSchemaType = SwaggerSchemaString } )
      , ("age", SwaggerInline mempty
            { swaggerSchemaType = SwaggerSchemaInteger
            , swaggerSchemaFormat = Just "int32"
            , swaggerSchemaCommon = mempty
                { swaggerSchemaMinimum = Just 0 } } ) ] }

schemaSimpleModelExampleJSON :: Value
schemaSimpleModelExampleJSON = [aesonQQ|
{
  "type": "object",
  "required": [
    "name"
  ],
  "properties": {
    "name": {
      "type": "string"
    },
    "age": {
      "type": "integer",
      "format": "int32",
      "minimum": 0
    }
  }
}
|]

schemaModelDictExample :: SwaggerSchema
schemaModelDictExample = mempty
  { swaggerSchemaType = SwaggerSchemaObject
  , swaggerSchemaAdditionalProperties = Just mempty
      { swaggerSchemaType = SwaggerSchemaString } }

schemaModelDictExampleJSON :: Value
schemaModelDictExampleJSON = [aesonQQ|
{
  "type": "object",
  "additionalProperties": {
    "type": "string"
  }
}
|]

schemaWithExampleExample :: SwaggerSchema
schemaWithExampleExample = mempty
  { swaggerSchemaType = SwaggerSchemaObject
  , swaggerSchemaProperties =
      [ ("id", SwaggerInline mempty
            { swaggerSchemaType   = SwaggerSchemaInteger
            , swaggerSchemaFormat = Just "int64" })
      , ("name", SwaggerInline mempty
            { swaggerSchemaType = SwaggerSchemaString }) ]
  , swaggerSchemaRequired = [ "name" ]
  , swaggerSchemaExample = Just [aesonQQ|
      {
        "name": "Puma",
        "id": 1
      }
    |] }

schemaWithExampleExampleJSON :: Value
schemaWithExampleExampleJSON = [aesonQQ|
{
  "type": "object",
  "properties": {
    "id": {
      "type": "integer",
      "format": "int64"
    },
    "name": {
      "type": "string"
    }
  },
  "required": [
    "name"
  ],
  "example": {
    "name": "Puma",
    "id": 1
  }
}
|]

-- =======================================================================
-- Definitions object
-- =======================================================================

definitionsExample :: HashMap Text SwaggerSchema
definitionsExample =
  [ ("Category", mempty
      { swaggerSchemaType = SwaggerSchemaObject
      , swaggerSchemaProperties =
          [ ("id", SwaggerInline mempty
              { swaggerSchemaType = SwaggerSchemaInteger
              , swaggerSchemaFormat = Just "int64" })
          , ("name", SwaggerInline mempty
              { swaggerSchemaType = SwaggerSchemaString }) ] })
  , ("Tag", mempty
      { swaggerSchemaType = SwaggerSchemaObject
      , swaggerSchemaProperties =
          [ ("id", SwaggerInline mempty
              { swaggerSchemaType = SwaggerSchemaInteger
              , swaggerSchemaFormat = Just "int64" })
          , ("name", SwaggerInline mempty
              { swaggerSchemaType = SwaggerSchemaString }) ] }) ]

definitionsExampleJSON :: Value
definitionsExampleJSON = [aesonQQ|
{
  "Category": {
    "type": "object",
    "properties": {
      "id": {
        "type": "integer",
        "format": "int64"
      },
      "name": {
        "type": "string"
      }
    }
  },
  "Tag": {
    "type": "object",
    "properties": {
      "id": {
        "type": "integer",
        "format": "int64"
      },
      "name": {
        "type": "string"
      }
    }
  }
}
|]

-- =======================================================================
-- Parameters Definition object
-- =======================================================================

parametersDefinitionExample :: HashMap Text SwaggerParameter
parametersDefinitionExample =
  [ ("skipParam", mempty
      { swaggerParameterName = "skip"
      , swaggerParameterDescription = Just "number of items to skip"
      , swaggerParameterRequired = True
      , swaggerParameterSchema = SwaggerParameterOther mempty
          { swaggerParameterOtherSchemaIn = SwaggerParameterQuery
          , swaggerParameterOtherSchemaType = SwaggerParamInteger
          , swaggerParameterOtherSchemaFormat = Just "int32" } })
  , ("limitParam", mempty
      { swaggerParameterName = "limit"
      , swaggerParameterDescription = Just "max records to return"
      , swaggerParameterRequired = True
      , swaggerParameterSchema = SwaggerParameterOther mempty
          { swaggerParameterOtherSchemaIn = SwaggerParameterQuery
          , swaggerParameterOtherSchemaType = SwaggerParamInteger
          , swaggerParameterOtherSchemaFormat = Just "int32" } }) ]

parametersDefinitionExampleJSON :: Value
parametersDefinitionExampleJSON = [aesonQQ|
{
  "skipParam": {
    "name": "skip",
    "in": "query",
    "description": "number of items to skip",
    "required": true,
    "type": "integer",
    "format": "int32"
  },
  "limitParam": {
    "name": "limit",
    "in": "query",
    "description": "max records to return",
    "required": true,
    "type": "integer",
    "format": "int32"
  }
}
|]

-- =======================================================================
-- Responses Definition object
-- =======================================================================

responsesDefinitionExample :: HashMap Text SwaggerResponse
responsesDefinitionExample =
  [ ("NotFound", mempty { swaggerResponseDescription = "Entity not found." })
  , ("IllegalInput", mempty { swaggerResponseDescription = "Illegal input for operation." }) ]

responsesDefinitionExampleJSON :: Value
responsesDefinitionExampleJSON = [aesonQQ|
{
  "NotFound": {
    "description": "Entity not found."
  },
  "IllegalInput": {
    "description": "Illegal input for operation."
  }
}
|]

-- =======================================================================
-- Responses Definition object
-- =======================================================================

securityDefinitionsExample :: HashMap Text SwaggerSecurityScheme
securityDefinitionsExample =
  [ ("api_key", SwaggerSecurityScheme
      { swaggerSecuritySchemeType = SwaggerSecuritySchemeApiKey (SwaggerApiKeyParams "api_key" SwaggerApiKeyHeader)
      , swaggerSecuritySchemeDescription = Nothing })
  , ("petstore_auth", SwaggerSecurityScheme
      { swaggerSecuritySchemeType = SwaggerSecuritySchemeOAuth2 (SwaggerOAuth2Params
          { swaggerOAuth2Flow = SwaggerOAuth2Implicit "http://swagger.io/api/oauth/dialog"
          , swaggerOAuth2Scopes =
              [ ("write:pets",  "modify pets in your account")
              , ("read:pets", "read your pets") ] } )
      , swaggerSecuritySchemeDescription = Nothing }) ]

securityDefinitionsExampleJSON :: Value
securityDefinitionsExampleJSON = [aesonQQ|
{
  "api_key": {
    "type": "apiKey",
    "name": "api_key",
    "in": "header"
  },
  "petstore_auth": {
    "type": "oauth2",
    "authorizationUrl": "http://swagger.io/api/oauth/dialog",
    "flow": "implicit",
    "scopes": {
      "write:pets": "modify pets in your account",
      "read:pets": "read your pets"
    }
  }
}
|]

-- =======================================================================
-- Swagger object
-- =======================================================================

swaggerExample :: Swagger
swaggerExample = mempty
  { swaggerBasePath = Just "/"
  , swaggerSchemes = Just [Http]
  , swaggerInfo = mempty
      { swaggerInfoVersion = "1.0"
      , swaggerInfoTitle = "Todo API"
      , swaggerInfoLicense = Just SwaggerLicense
          { swaggerLicenseName = "MIT"
          , swaggerLicenseUrl = Just (URL "http://mit.com") }
      , swaggerInfoDescription = Just "This is a an API that tests servant-swagger support for a Todo API" }
  , swaggerPaths = mempty
      { swaggerPathsMap =
          [ ("/todo/{id}", mempty
              { swaggerPathItemGet = Just mempty
                  { swaggerOperationResponses = mempty
                      { swaggerResponsesResponses =
                          [ (200, SwaggerInline mempty
                              { swaggerResponseSchema = Just mempty
                                { swaggerSchemaExample = Just [aesonQQ|
                                    {
                                      "created": 100,
                                      "description": "get milk"
                                    } |]
                                , swaggerSchemaType = SwaggerSchemaObject
                                , swaggerSchemaDescription = Just "This is some real Todo right here"
                                , swaggerSchemaProperties =
                                    [ ("created", SwaggerInline mempty
                                        { swaggerSchemaType = SwaggerSchemaInteger
                                        , swaggerSchemaFormat = Just "int32" })
                                    , ("description", SwaggerInline mempty
                                        { swaggerSchemaType = SwaggerSchemaString }) ] }
                              , swaggerResponseDescription = "OK" }) ] }
                  , swaggerOperationProduces = Just (SwaggerMimeList [ "application/json" ])
                  , swaggerOperationParameters =
                      [ SwaggerInline mempty
                          { swaggerParameterRequired = True
                          , swaggerParameterName = "id"
                          , swaggerParameterDescription = Just "TodoId param"
                          , swaggerParameterSchema = SwaggerParameterOther mempty
                              { swaggerParameterOtherSchemaIn = SwaggerParameterPath
                              , swaggerParameterOtherSchemaType = SwaggerParamString } } ]
                  , swaggerOperationTags = [ "todo" ] } }) ] } }

swaggerExampleJSON :: Value
swaggerExampleJSON = [aesonQQ|
{
    "swagger": "2.0",
    "basePath": "/",
    "schemes": [
        "http"
    ],
    "info": {
        "version": "1.0",
        "title": "Todo API",
        "license": {
            "url": "http://mit.com",
            "name": "MIT"
        },
        "description": "This is a an API that tests servant-swagger support for a Todo API"
    },
    "paths": {
        "/todo/{id}": {
            "get": {
                "responses": {
                    "200": {
                        "schema": {
                            "example": {
                                "created": 100,
                                "description": "get milk"
                            },
                            "type": "object",
                            "description": "This is some real Todo right here",
                            "properties": {
                                "created": {
                                    "format": "int32",
                                    "type": "integer"
                                },
                                "description": {
                                    "type": "string"
                                }
                            }
                        },
                        "description": "OK"
                    }
                },
                "produces": [
                    "application/json"
                ],
                "parameters": [
                    {
                        "required": true,
                        "in": "path",
                        "name": "id",
                        "type": "string",
                        "description": "TodoId param"
                    }
                ],
                "tags": [
                    "todo"
                ]
            }
        }
    }
}
|]
