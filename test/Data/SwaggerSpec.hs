{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  describe "Swagger Object" $ do
    context "Todo Example" $ swaggerExample <~> swaggerExampleJSON
    context "PetStore Example" $
      it "decodes successfully" $ do
        fromJSON petstoreExampleJSON `shouldSatisfy` (\x -> case x of Success (_ :: Swagger) -> True; _ -> False)

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
      , ("address", SwaggerRef (SwaggerReference "#/definitions/Address"))
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
    "address": {
      "$ref": "#/definitions/Address"
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
                              { swaggerResponseSchema = Just $ SwaggerInline mempty
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

petstoreExampleJSON :: Value
petstoreExampleJSON = [aesonQQ|
{  
   "swagger":"2.0",
   "info":{  
      "description":"This is a sample server Petstore server.  You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/).  For this sample, you can use the api key `special-key` to test the authorization filters.",
      "version":"1.0.0",
      "title":"Swagger Petstore",
      "termsOfService":"http://swagger.io/terms/",
      "contact":{  
         "email":"apiteam@swagger.io"
      },
      "license":{  
         "name":"Apache 2.0",
         "url":"http://www.apache.org/licenses/LICENSE-2.0.html"
      }
   },
   "host":"petstore.swagger.io",
   "basePath":"/v2",
   "tags":[  
      {  
         "name":"pet",
         "description":"Everything about your Pets",
         "externalDocs":{  
            "description":"Find out more",
            "url":"http://swagger.io"
         }
      },
      {  
         "name":"store",
         "description":"Access to Petstore orders"
      },
      {  
         "name":"user",
         "description":"Operations about user",
         "externalDocs":{  
            "description":"Find out more about our store",
            "url":"http://swagger.io"
         }
      }
   ],
   "schemes":[  
      "http"
   ],
   "paths":{  
      "/pet":{  
         "post":{  
            "tags":[  
               "pet"
            ],
            "summary":"Add a new pet to the store",
            "description":"",
            "operationId":"addPet",
            "consumes":[  
               "application/json",
               "application/xml"
            ],
            "produces":[  
               "application/xml",
               "application/json"
            ],
            "parameters":[  
               {  
                  "in":"body",
                  "name":"body",
                  "description":"Pet object that needs to be added to the store",
                  "required":true,
                  "schema":{  
                     "$ref":"#/definitions/Pet"
                  }
               }
            ],
            "responses":{  
               "405":{  
                  "description":"Invalid input"
               }
            },
            "security":[  
               {  
                  "petstore_auth":[  
                     "write:pets",
                     "read:pets"
                  ]
               }
            ]
         },
         "put":{  
            "tags":[  
               "pet"
            ],
            "summary":"Update an existing pet",
            "description":"",
            "operationId":"updatePet",
            "consumes":[  
               "application/json",
               "application/xml"
            ],
            "produces":[  
               "application/xml",
               "application/json"
            ],
            "parameters":[  
               {  
                  "in":"body",
                  "name":"body",
                  "description":"Pet object that needs to be added to the store",
                  "required":true,
                  "schema":{  
                     "$ref":"#/definitions/Pet"
                  }
               }
            ],
            "responses":{  
               "400":{  
                  "description":"Invalid ID supplied"
               },
               "404":{  
                  "description":"Pet not found"
               },
               "405":{  
                  "description":"Validation exception"
               }
            },
            "security":[  
               {  
                  "petstore_auth":[  
                     "write:pets",
                     "read:pets"
                  ]
               }
            ]
         }
      },
      "/pet/findByStatus":{  
         "get":{  
            "tags":[  
               "pet"
            ],
            "summary":"Finds Pets by status",
            "description":"Multiple status values can be provided with comma seperated strings",
            "operationId":"findPetsByStatus",
            "produces":[  
               "application/xml",
               "application/json"
            ],
            "parameters":[  
               {  
                  "name":"status",
                  "in":"query",
                  "description":"Status values that need to be considered for filter",
                  "required":true,
                  "type":"array",
                  "items":{  
                     "type":"string",
                     "enum":[  
                        "available",
                        "pending",
                        "sold"
                     ],
                     "default":"available"
                  },
                  "collectionFormat":"csv"
               }
            ],
            "responses":{  
               "200":{  
                  "description":"successful operation",
                  "schema":{  
                     "type":"array",
                     "items":{  
                        "$ref":"#/definitions/Pet"
                     }
                  }
               },
               "400":{  
                  "description":"Invalid status value"
               }
            },
            "security":[  
               {  
                  "petstore_auth":[  
                     "write:pets",
                     "read:pets"
                  ]
               }
            ]
         }
      },
      "/pet/findByTags":{  
         "get":{  
            "tags":[  
               "pet"
            ],
            "summary":"Finds Pets by tags",
            "description":"Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.",
            "operationId":"findPetsByTags",
            "produces":[  
               "application/xml",
               "application/json"
            ],
            "parameters":[  
               {  
                  "name":"tags",
                  "in":"query",
                  "description":"Tags to filter by",
                  "required":true,
                  "type":"array",
                  "items":{  
                     "type":"string"
                  },
                  "collectionFormat":"csv"
               }
            ],
            "responses":{  
               "200":{  
                  "description":"successful operation",
                  "schema":{  
                     "type":"array",
                     "items":{  
                        "$ref":"#/definitions/Pet"
                     }
                  }
               },
               "400":{  
                  "description":"Invalid tag value"
               }
            },
            "security":[  
               {  
                  "petstore_auth":[  
                     "write:pets",
                     "read:pets"
                  ]
               }
            ]
         }
      },
      "/pet/{petId}":{  
         "get":{  
            "tags":[  
               "pet"
            ],
            "summary":"Find pet by ID",
            "description":"Returns a single pet",
            "operationId":"getPetById",
            "produces":[  
               "application/xml",
               "application/json"
            ],
            "parameters":[  
               {  
                  "name":"petId",
                  "in":"path",
                  "description":"ID of pet to return",
                  "required":true,
                  "type":"integer",
                  "format":"int64"
               }
            ],
            "responses":{  
               "200":{  
                  "description":"successful operation",
                  "schema":{  
                     "$ref":"#/definitions/Pet"
                  }
               },
               "400":{  
                  "description":"Invalid ID supplied"
               },
               "404":{  
                  "description":"Pet not found"
               }
            },
            "security":[  
               {  
                  "api_key":[  

                  ]
               }
            ]
         },
         "post":{  
            "tags":[  
               "pet"
            ],
            "summary":"Updates a pet in the store with form data",
            "description":"",
            "operationId":"updatePetWithForm",
            "consumes":[  
               "application/x-www-form-urlencoded"
            ],
            "produces":[  
               "application/xml",
               "application/json"
            ],
            "parameters":[  
               {  
                  "name":"petId",
                  "in":"path",
                  "description":"ID of pet that needs to be updated",
                  "required":true,
                  "type":"integer",
                  "format":"int64"
               },
               {  
                  "name":"name",
                  "in":"formData",
                  "description":"Updated name of the pet",
                  "required":false,
                  "type":"string"
               },
               {  
                  "name":"status",
                  "in":"formData",
                  "description":"Updated status of the pet",
                  "required":false,
                  "type":"string"
               }
            ],
            "responses":{  
               "405":{  
                  "description":"Invalid input"
               }
            },
            "security":[  
               {  
                  "petstore_auth":[  
                     "write:pets",
                     "read:pets"
                  ]
               }
            ]
         },
         "delete":{  
            "tags":[  
               "pet"
            ],
            "summary":"Deletes a pet",
            "description":"",
            "operationId":"deletePet",
            "produces":[  
               "application/xml",
               "application/json"
            ],
            "parameters":[  
               {  
                  "name":"api_key",
                  "in":"header",
                  "required":false,
                  "type":"string"
               },
               {  
                  "name":"petId",
                  "in":"path",
                  "description":"Pet id to delete",
                  "required":true,
                  "type":"integer",
                  "format":"int64"
               }
            ],
            "responses":{  
               "400":{  
                  "description":"Invalid pet value"
               }
            },
            "security":[  
               {  
                  "petstore_auth":[  
                     "write:pets",
                     "read:pets"
                  ]
               }
            ]
         }
      },
      "/pet/{petId}/uploadImage":{  
         "post":{  
            "tags":[  
               "pet"
            ],
            "summary":"uploads an image",
            "description":"",
            "operationId":"uploadFile",
            "consumes":[  
               "multipart/form-data"
            ],
            "produces":[  
               "application/json"
            ],
            "parameters":[  
               {  
                  "name":"petId",
                  "in":"path",
                  "description":"ID of pet to update",
                  "required":true,
                  "type":"integer",
                  "format":"int64"
               },
               {  
                  "name":"additionalMetadata",
                  "in":"formData",
                  "description":"Additional data to pass to server",
                  "required":false,
                  "type":"string"
               },
               {  
                  "name":"file",
                  "in":"formData",
                  "description":"file to upload",
                  "required":false,
                  "type":"file"
               }
            ],
            "responses":{  
               "200":{  
                  "description":"successful operation",
                  "schema":{  
                     "$ref":"#/definitions/ApiResponse"
                  }
               }
            },
            "security":[  
               {  
                  "petstore_auth":[  
                     "write:pets",
                     "read:pets"
                  ]
               }
            ]
         }
      },
      "/store/inventory":{  
         "get":{  
            "tags":[  
               "store"
            ],
            "summary":"Returns pet inventories by status",
            "description":"Returns a map of status codes to quantities",
            "operationId":"getInventory",
            "produces":[  
               "application/json"
            ],
            "parameters":[  

            ],
            "responses":{  
               "200":{  
                  "description":"successful operation",
                  "schema":{  
                     "type":"object",
                     "additionalProperties":{  
                        "type":"integer",
                        "format":"int32"
                     }
                  }
               }
            },
            "security":[  
               {  
                  "api_key":[  

                  ]
               }
            ]
         }
      },
      "/store/order":{  
         "post":{  
            "tags":[  
               "store"
            ],
            "summary":"Place an order for a pet",
            "description":"",
            "operationId":"placeOrder",
            "produces":[  
               "application/xml",
               "application/json"
            ],
            "parameters":[  
               {  
                  "in":"body",
                  "name":"body",
                  "description":"order placed for purchasing the pet",
                  "required":true,
                  "schema":{  
                     "$ref":"#/definitions/Order"
                  }
               }
            ],
            "responses":{  
               "200":{  
                  "description":"successful operation",
                  "schema":{  
                     "$ref":"#/definitions/Order"
                  }
               },
               "400":{  
                  "description":"Invalid Order"
               }
            }
         }
      },
      "/store/order/{orderId}":{  
         "get":{  
            "tags":[  
               "store"
            ],
            "summary":"Find purchase order by ID",
            "description":"For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions",
            "operationId":"getOrderById",
            "produces":[  
               "application/xml",
               "application/json"
            ],
            "parameters":[  
               {  
                  "name":"orderId",
                  "in":"path",
                  "description":"ID of pet that needs to be fetched",
                  "required":true,
                  "type":"integer",
                  "maximum":5.0,
                  "minimum":1.0,
                  "format":"int64"
               }
            ],
            "responses":{  
               "200":{  
                  "description":"successful operation",
                  "schema":{  
                     "$ref":"#/definitions/Order"
                  }
               },
               "400":{  
                  "description":"Invalid ID supplied"
               },
               "404":{  
                  "description":"Order not found"
               }
            }
         },
         "delete":{  
            "tags":[  
               "store"
            ],
            "summary":"Delete purchase order by ID",
            "description":"For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors",
            "operationId":"deleteOrder",
            "produces":[  
               "application/xml",
               "application/json"
            ],
            "parameters":[  
               {  
                  "name":"orderId",
                  "in":"path",
                  "description":"ID of the order that needs to be deleted",
                  "required":true,
                  "type":"string",
                  "minimum":1.0
               }
            ],
            "responses":{  
               "400":{  
                  "description":"Invalid ID supplied"
               },
               "404":{  
                  "description":"Order not found"
               }
            }
         }
      },
      "/user":{  
         "post":{  
            "tags":[  
               "user"
            ],
            "summary":"Create user",
            "description":"This can only be done by the logged in user.",
            "operationId":"createUser",
            "produces":[  
               "application/xml",
               "application/json"
            ],
            "parameters":[  
               {  
                  "in":"body",
                  "name":"body",
                  "description":"Created user object",
                  "required":true,
                  "schema":{  
                     "$ref":"#/definitions/User"
                  }
               }
            ],
            "responses":{  
               "default":{  
                  "description":"successful operation"
               }
            }
         }
      },
      "/user/createWithArray":{  
         "post":{  
            "tags":[  
               "user"
            ],
            "summary":"Creates list of users with given input array",
            "description":"",
            "operationId":"createUsersWithArrayInput",
            "produces":[  
               "application/xml",
               "application/json"
            ],
            "parameters":[  
               {  
                  "in":"body",
                  "name":"body",
                  "description":"List of user object",
                  "required":true,
                  "schema":{  
                     "type":"array",
                     "items":{  
                        "$ref":"#/definitions/User"
                     }
                  }
               }
            ],
            "responses":{  
               "default":{  
                  "description":"successful operation"
               }
            }
         }
      },
      "/user/createWithList":{  
         "post":{  
            "tags":[  
               "user"
            ],
            "summary":"Creates list of users with given input array",
            "description":"",
            "operationId":"createUsersWithListInput",
            "produces":[  
               "application/xml",
               "application/json"
            ],
            "parameters":[  
               {  
                  "in":"body",
                  "name":"body",
                  "description":"List of user object",
                  "required":true,
                  "schema":{  
                     "type":"array",
                     "items":{  
                        "$ref":"#/definitions/User"
                     }
                  }
               }
            ],
            "responses":{  
               "default":{  
                  "description":"successful operation"
               }
            }
         }
      },
      "/user/login":{  
         "get":{  
            "tags":[  
               "user"
            ],
            "summary":"Logs user into the system",
            "description":"",
            "operationId":"loginUser",
            "produces":[  
               "application/xml",
               "application/json"
            ],
            "parameters":[  
               {  
                  "name":"username",
                  "in":"query",
                  "description":"The user name for login",
                  "required":true,
                  "type":"string"
               },
               {  
                  "name":"password",
                  "in":"query",
                  "description":"The password for login in clear text",
                  "required":true,
                  "type":"string"
               }
            ],
            "responses":{  
               "200":{  
                  "description":"successful operation",
                  "schema":{  
                     "type":"string"
                  },
                  "headers":{  
                     "X-Rate-Limit":{  
                        "type":"integer",
                        "format":"int32",
                        "description":"calls per hour allowed by the user"
                     },
                     "X-Expires-After":{  
                        "type":"string",
                        "format":"date-time",
                        "description":"date in UTC when toekn expires"
                     }
                  }
               },
               "400":{  
                  "description":"Invalid username/password supplied"
               }
            }
         }
      },
      "/user/logout":{  
         "get":{  
            "tags":[  
               "user"
            ],
            "summary":"Logs out current logged in user session",
            "description":"",
            "operationId":"logoutUser",
            "produces":[  
               "application/xml",
               "application/json"
            ],
            "parameters":[  

            ],
            "responses":{  
               "default":{  
                  "description":"successful operation"
               }
            }
         }
      },
      "/user/{username}":{  
         "get":{  
            "tags":[  
               "user"
            ],
            "summary":"Get user by user name",
            "description":"",
            "operationId":"getUserByName",
            "produces":[  
               "application/xml",
               "application/json"
            ],
            "parameters":[  
               {  
                  "name":"username",
                  "in":"path",
                  "description":"The name that needs to be fetched. Use user1 for testing. ",
                  "required":true,
                  "type":"string"
               }
            ],
            "responses":{  
               "200":{  
                  "description":"successful operation",
                  "schema":{  
                     "$ref":"#/definitions/User"
                  }
               },
               "400":{  
                  "description":"Invalid username supplied"
               },
               "404":{  
                  "description":"User not found"
               }
            }
         },
         "put":{  
            "tags":[  
               "user"
            ],
            "summary":"Updated user",
            "description":"This can only be done by the logged in user.",
            "operationId":"updateUser",
            "produces":[  
               "application/xml",
               "application/json"
            ],
            "parameters":[  
               {  
                  "name":"username",
                  "in":"path",
                  "description":"name that need to be deleted",
                  "required":true,
                  "type":"string"
               },
               {  
                  "in":"body",
                  "name":"body",
                  "description":"Updated user object",
                  "required":true,
                  "schema":{  
                     "$ref":"#/definitions/User"
                  }
               }
            ],
            "responses":{  
               "400":{  
                  "description":"Invalid user supplied"
               },
               "404":{  
                  "description":"User not found"
               }
            }
         },
         "delete":{  
            "tags":[  
               "user"
            ],
            "summary":"Delete user",
            "description":"This can only be done by the logged in user.",
            "operationId":"deleteUser",
            "produces":[  
               "application/xml",
               "application/json"
            ],
            "parameters":[  
               {  
                  "name":"username",
                  "in":"path",
                  "description":"The name that needs to be deleted",
                  "required":true,
                  "type":"string"
               }
            ],
            "responses":{  
               "400":{  
                  "description":"Invalid username supplied"
               },
               "404":{  
                  "description":"User not found"
               }
            }
         }
      }
   },
   "securityDefinitions":{  
      "petstore_auth":{  
         "type":"oauth2",
         "authorizationUrl":"http://petstore.swagger.io/api/oauth/dialog",
         "flow":"implicit",
         "scopes":{  
            "write:pets":"modify pets in your account",
            "read:pets":"read your pets"
         }
      },
      "api_key":{  
         "type":"apiKey",
         "name":"api_key",
         "in":"header"
      }
   },
   "definitions":{  
      "Order":{  
         "type":"object",
         "properties":{  
            "id":{  
               "type":"integer",
               "format":"int64"
            },
            "petId":{  
               "type":"integer",
               "format":"int64"
            },
            "quantity":{  
               "type":"integer",
               "format":"int32"
            },
            "shipDate":{  
               "type":"string",
               "format":"date-time"
            },
            "status":{  
               "type":"string",
               "description":"Order Status",
               "enum":[  
                  "placed",
                  "approved",
                  "delivered"
               ]
            },
            "complete":{  
               "type":"boolean",
               "default":false
            }
         },
         "xml":{  
            "name":"Order"
         }
      },
      "Category":{  
         "type":"object",
         "properties":{  
            "id":{  
               "type":"integer",
               "format":"int64"
            },
            "name":{  
               "type":"string"
            }
         },
         "xml":{  
            "name":"Category"
         }
      },
      "User":{  
         "type":"object",
         "properties":{  
            "id":{  
               "type":"integer",
               "format":"int64"
            },
            "username":{  
               "type":"string"
            },
            "firstName":{  
               "type":"string"
            },
            "lastName":{  
               "type":"string"
            },
            "email":{  
               "type":"string"
            },
            "password":{  
               "type":"string"
            },
            "phone":{  
               "type":"string"
            },
            "userStatus":{  
               "type":"integer",
               "format":"int32",
               "description":"User Status"
            }
         },
         "xml":{  
            "name":"User"
         }
      },
      "Tag":{  
         "type":"object",
         "properties":{  
            "id":{  
               "type":"integer",
               "format":"int64"
            },
            "name":{  
               "type":"string"
            }
         },
         "xml":{  
            "name":"Tag"
         }
      },
      "Pet":{  
         "type":"object",
         "required":[  
            "name",
            "photoUrls"
         ],
         "properties":{  
            "id":{  
               "type":"integer",
               "format":"int64"
            },
            "category":{  
               "$ref":"#/definitions/Category"
            },
            "name":{  
               "type":"string",
               "example":"doggie"
            },
            "photoUrls":{  
               "type":"array",
               "xml":{  
                  "name":"photoUrl",
                  "wrapped":true
               },
               "items":{  
                  "type":"string"
               }
            },
            "tags":{  
               "type":"array",
               "xml":{  
                  "name":"tag",
                  "wrapped":true
               },
               "items":{  
                  "$ref":"#/definitions/Tag"
               }
            },
            "status":{  
               "type":"string",
               "description":"pet status in the store",
               "enum":[  
                  "available",
                  "pending",
                  "sold"
               ]
            }
         },
         "xml":{  
            "name":"Pet"
         }
      },
      "ApiResponse":{  
         "type":"object",
         "properties":{  
            "code":{  
               "type":"integer",
               "format":"int32"
            },
            "type":{  
               "type":"string"
            },
            "message":{  
               "type":"string"
            }
         }
      }
   },
   "externalDocs":{  
      "description":"Find out more about Swagger",
      "url":"http://swagger.io"
   }
}
|]
