{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.SwaggerSpec where

import Prelude ()
import Prelude.Compat

import Control.Lens

import Data.Aeson
import Data.Aeson.QQ.Simple
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet.InsOrd as InsOrdHS
import Data.Text (Text)

import Data.Swagger
import SpecCommon
import Test.Hspec hiding (example)

spec :: Spec
spec = do
  describe "License Object" $ licenseExample <=> licenseExampleJSON
  describe "Contact Object" $ contactExample <=> contactExampleJSON
  describe "Info Object" $ infoExample <=> infoExampleJSON
  describe "Operation Object" $ operationExample <=> operationExampleJSON
  describe "Schema Object" $ do
    context "Primitive Sample" $ schemaPrimitiveExample <=> schemaPrimitiveExampleJSON
    context "Simple Model" $ schemaSimpleModelExample <=> schemaSimpleModelExampleJSON
    context "Model with Map/Dictionary Properties" $ schemaModelDictExample <=> schemaModelDictExampleJSON
    context "Model with Arbitrary Properties" $ schemaAdditionalExample <=> schemaAdditionalExampleJSON
    context "Model with Example" $ schemaWithExampleExample <=> schemaWithExampleExampleJSON
  describe "Definitions Object" $ definitionsExample <=> definitionsExampleJSON
  describe "Parameters Definition Object" $ paramsDefinitionExample <=> paramsDefinitionExampleJSON
  describe "Responses Definition Object" $ responsesDefinitionExample <=> responsesDefinitionExampleJSON
  describe "Security Definitions Object" $ securityDefinitionsExample <=> securityDefinitionsExampleJSON
  describe "OAuth2 Security Definitions with merged Scope" $ oAuth2SecurityDefinitionsExample <=> oAuth2SecurityDefinitionsExampleJSON
  describe "Composition Schema Example" $ compositionSchemaExample <=> compositionSchemaExampleJSON
  describe "Swagger Object" $ do
    context "Example with no paths" $ emptyPathsFieldExample <=> emptyPathsFieldExampleJSON
    context "Todo Example" $ swaggerExample <=> swaggerExampleJSON
    context "PetStore Example" $ do
      it "decodes successfully" $ do
        fromJSON petstoreExampleJSON `shouldSatisfy` (\x -> case x of Success (_ :: Swagger) -> True; _ -> False)
      it "roundtrips: fmap toJSON . fromJSON" $ do
        (toJSON :: Swagger -> Value) <$> fromJSON petstoreExampleJSON `shouldBe` Success petstoreExampleJSON

main :: IO ()
main = hspec spec

-- =======================================================================
-- Info object
-- =======================================================================

infoExample :: Info
infoExample = mempty
  & title          .~ "Swagger Sample App"
  & description    ?~ "This is a sample server Petstore server."
  & termsOfService ?~ "http://swagger.io/terms/"
  & contact        ?~ contactExample
  & license        ?~ licenseExample
  & version        .~ "1.0.1"

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

contactExample :: Contact
contactExample = mempty
  & name  ?~ "API Support"
  & url   ?~ URL "http://www.swagger.io/support"
  & email ?~ "support@swagger.io"

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

licenseExample :: License
licenseExample = "Apache 2.0"
  & url ?~ URL "http://www.apache.org/licenses/LICENSE-2.0.html"

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

operationExample :: Operation
operationExample = mempty
  & tags    .~ InsOrdHS.fromList ["pet"]
  & summary ?~ "Updates a pet in the store with form data"
  & description ?~ ""
  & operationId ?~ "updatePetWithForm"
  & parameters .~ [Inline (mempty
    & name .~ "petId"
    & description ?~ "ID of pet that needs to be updated"
    & required ?~ True
    & in_ .~ ParamPath
    & schema ?~ Inline (mempty & type_ ?~ SwaggerString))]
  & requestBody ?~ Inline (
    mempty & content . at "application/x-www-form-urlencoded" ?~ (mempty & schema ?~ (Inline (mempty
      & properties . at "petId" ?~ Inline (mempty
        & description ?~ "Updated name of the pet"
        & type_ ?~ SwaggerString)
      & properties . at "status" ?~ Inline (mempty
        & description ?~ "Updated status of the pet"
        & type_ ?~ SwaggerString)))))
  & at 200 ?~ "Pet updated."
  & at 405 ?~ "Invalid input"
  & security .~ [SecurityRequirement [("petstore_auth", ["write:pets", "read:pets"])]]

operationExampleJSON :: Value
operationExampleJSON = [aesonQQ|
{
  "tags": [
    "pet"
  ],
  "summary": "Updates a pet in the store with form data",
  "description": "",
  "operationId": "updatePetWithForm",
  "parameters": [
    {
      "required": true,
      "schema": {
        "type": "string"
      },
      "in": "path",
      "name": "petId",
      "description": "ID of pet that needs to be updated"
    }
  ],
  "requestBody": {
    "content": {
      "application/x-www-form-urlencoded": {
        "schema": {
          "properties": {
            "petId": {
              "type": "string",
              "description": "Updated name of the pet"
            },
            "status": {
              "type": "string",
              "description": "Updated status of the pet"
            }
          }
        }
      }
    }
  },
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

schemaPrimitiveExample :: Schema
schemaPrimitiveExample = mempty
  & type_  ?~ SwaggerString
  & format ?~ "email"

schemaPrimitiveExampleJSON :: Value
schemaPrimitiveExampleJSON = [aesonQQ|
{
    "type": "string",
    "format": "email"
}
|]

schemaSimpleModelExample :: Schema
schemaSimpleModelExample = mempty
  & type_ ?~ SwaggerObject
  & required .~ [ "name" ]
  & properties .~
      [ ("name", Inline (mempty & type_ ?~ SwaggerString))
      , ("address", Ref (Reference "Address"))
      , ("age", Inline $ mempty
            & minimum_ ?~ 0
            & type_    ?~ SwaggerInteger
            & format   ?~ "int32" ) ]

schemaSimpleModelExampleJSON :: Value
schemaSimpleModelExampleJSON = [aesonQQ|
{  "required": [    "name"  ],
  "properties": {
    "name": {
      "type": "string"
    },
    "address": {
      "$ref": "#/components/schemas/Address"
    },
    "age": {
      "format": "int32",
      "minimum": 0,
      "type": "integer"
    }
  },
  "type": "object"
}
|]

schemaModelDictExample :: Schema
schemaModelDictExample = mempty
  & type_ ?~ SwaggerObject
  & additionalProperties ?~ AdditionalPropertiesSchema (Inline (mempty & type_ ?~ SwaggerString))

schemaModelDictExampleJSON :: Value
schemaModelDictExampleJSON = [aesonQQ|
{
  "type": "object",
  "additionalProperties": {
    "type": "string"
  }
}
|]

schemaAdditionalExample :: Schema
schemaAdditionalExample = mempty
  & type_ ?~ SwaggerObject
  & additionalProperties ?~ AdditionalPropertiesAllowed True

schemaAdditionalExampleJSON :: Value
schemaAdditionalExampleJSON = [aesonQQ|
{
  "type": "object",
  "additionalProperties": true
}
|]

schemaWithExampleExample :: Schema
schemaWithExampleExample = mempty
  & type_ ?~ SwaggerObject
  & properties .~
      [ ("id", Inline $ mempty
            & type_  ?~ SwaggerInteger
            & format ?~ "int64" )
      , ("name", Inline $ mempty
            & type_ ?~ SwaggerString) ]
  & required .~ [ "name" ]
  & example ?~ [aesonQQ|
    {
      "name": "Puma",
      "id": 1
    }
  |]

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

definitionsExample :: HashMap Text Schema
definitionsExample =
  [ ("Category", mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("id", Inline $ mempty
              & type_  ?~ SwaggerInteger
              & format ?~ "int64")
          , ("name", Inline (mempty & type_ ?~ SwaggerString)) ] )
  , ("Tag", mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("id", Inline $ mempty
              & type_  ?~ SwaggerInteger
              & format ?~ "int64")
          , ("name", Inline (mempty & type_ ?~ SwaggerString)) ] ) ]

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

paramsDefinitionExample :: HashMap Text Param
paramsDefinitionExample =
  [ ("skipParam", mempty
      & name .~ "skip"
      & description ?~ "number of items to skip"
      & required ?~ True
      & in_    .~ ParamQuery
      & schema ?~ Inline (mempty
          & type_  ?~ SwaggerInteger
          & format ?~ "int32" ))
  , ("limitParam", mempty
      & name .~ "limit"
      & description ?~ "max records to return"
      & required ?~ True
      & in_    .~ ParamQuery
      & schema ?~ Inline (mempty
          & type_  ?~ SwaggerInteger
          & format ?~ "int32" )) ]

paramsDefinitionExampleJSON :: Value
paramsDefinitionExampleJSON = [aesonQQ|
{
  "skipParam": {
    "name": "skip",
    "in": "query",
    "description": "number of items to skip",
    "required": true,
    "schema": {
      "type": "integer",
      "format": "int32"
    }
  },
  "limitParam": {
    "name": "limit",
    "in": "query",
    "description": "max records to return",
    "required": true,
    "schema": {
      "type": "integer",
      "format": "int32"
    }
  }
}
|]

-- =======================================================================
-- Responses Definition object
-- =======================================================================

responsesDefinitionExample :: HashMap Text Response
responsesDefinitionExample =
  [ ("NotFound", mempty & description .~ "Entity not found.")
  , ("IllegalInput", mempty & description .~ "Illegal input for operation.") ]

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

securityDefinitionsExample :: SecurityDefinitions
securityDefinitionsExample = SecurityDefinitions
  [ ("api_key", SecurityScheme
      { _securitySchemeType = SecuritySchemeApiKey (ApiKeyParams "api_key" ApiKeyHeader)
      , _securitySchemeDescription = Nothing })
  , ("petstore_auth", SecurityScheme
      { _securitySchemeType = SecuritySchemeOAuth2 (mempty & implicit ?~ OAuth2Flow
            { _oAuth2Params = OAuth2ImplicitFlow "http://swagger.io/api/oauth/dialog"
            , _oAath2RefreshUrl = Nothing
            , _oAuth2Scopes =
                [ ("write:pets",  "modify pets in your account")
                , ("read:pets", "read your pets") ] } )
      , _securitySchemeDescription = Nothing }) ]

securityDefinitionsExampleJSON :: Value
securityDefinitionsExampleJSON = [aesonQQ|
{
  "api_key": {
    "in": "header",
    "name": "api_key",
    "type": "apiKey"
  },
  "petstore_auth": {
    "type": "oauth2",
    "flows": {
      "implicit": {
        "scopes": {
          "write:pets": "modify pets in your account",
          "read:pets": "read your pets"
        },
        "authorizationUrl": "http://swagger.io/api/oauth/dialog"
      }
    }
  }
}

|]

oAuth2SecurityDefinitionsReadExample :: SecurityDefinitions
oAuth2SecurityDefinitionsReadExample = SecurityDefinitions
  [ ("petstore_auth", SecurityScheme
      { _securitySchemeType = SecuritySchemeOAuth2 (mempty & implicit ?~ OAuth2Flow
            { _oAuth2Params = OAuth2ImplicitFlow "http://swagger.io/api/oauth/dialog"
            , _oAath2RefreshUrl = Nothing
            , _oAuth2Scopes =
              [ ("read:pets", "read your pets") ] } )
      , _securitySchemeDescription = Nothing })
  ]

oAuth2SecurityDefinitionsWriteExample :: SecurityDefinitions
oAuth2SecurityDefinitionsWriteExample = SecurityDefinitions
  [ ("petstore_auth", SecurityScheme
      { _securitySchemeType = SecuritySchemeOAuth2 (mempty & implicit ?~ OAuth2Flow
            { _oAuth2Params = OAuth2ImplicitFlow "http://swagger.io/api/oauth/dialog"
            , _oAath2RefreshUrl = Nothing
            , _oAuth2Scopes =
                [ ("write:pets", "modify pets in your account") ] } )
      , _securitySchemeDescription = Nothing })
  ]

oAuth2SecurityDefinitionsExample :: SecurityDefinitions
oAuth2SecurityDefinitionsExample =
  oAuth2SecurityDefinitionsWriteExample <>
  oAuth2SecurityDefinitionsReadExample

oAuth2SecurityDefinitionsExampleJSON :: Value
oAuth2SecurityDefinitionsExampleJSON = [aesonQQ|
{
  "petstore_auth": {
    "type": "oauth2",
    "flows": {
      "implicit": {
        "scopes": {
          "write:pets": "modify pets in your account",
          "read:pets": "read your pets"
        },
        "authorizationUrl": "http://swagger.io/api/oauth/dialog"
      }
    }
  }
}
|]

-- =======================================================================
-- Swagger object
-- =======================================================================

emptyPathsFieldExample :: Swagger
emptyPathsFieldExample = mempty

emptyPathsFieldExampleJSON :: Value
emptyPathsFieldExampleJSON = [aesonQQ|
{
  "openapi": "3.0.0",
  "info": {"version": "", "title": ""},
  "paths": {},
  "components": {}
}
|]

swaggerExample :: Swagger
swaggerExample = mempty
  -- & basePath ?~ "/"
  -- & schemes ?~ [Http]
  & info .~ (mempty
      & version .~ "1.0"
      & title .~ "Todo API"
      & license ?~ "MIT"
      & license._Just.url ?~ URL "http://mit.com"
      & description ?~ "This is an API that tests servant-swagger support for a Todo API")
  & paths.at "/todo/{id}" ?~ (mempty & get ?~ ((mempty :: Operation)
      & responses . at 200 ?~ Inline (mempty
          & description .~ "OK"
          & content . at "application/json" ?~ (mempty
              & schema ?~ Inline (mempty
                  & type_ ?~ SwaggerObject
                  & example ?~ [aesonQQ|
                      {
                        "created": 100,
                        "description": "get milk"
                      } |]
                  & description ?~ "This is some real Todo right here"
                  & properties .~
                     [ ("created", Inline $ mempty
                         & type_  ?~ SwaggerInteger
                         & format ?~ "int32")
                     , ("description", Inline (mempty & type_ ?~ SwaggerString))])))
      & parameters .~
          [ Inline $ mempty
              & required ?~ True
              & name .~ "id"
              & description ?~ "TodoId param"
              & in_ .~ ParamPath
              & schema ?~ Inline (mempty
                  & type_ ?~ SwaggerString ) ]
      & tags .~ InsOrdHS.fromList [ "todo" ] ))

swaggerExampleJSON :: Value
swaggerExampleJSON = [aesonQQ|
{
    "openapi": "3.0.0",
    "info": {
        "version": "1.0",
        "title": "Todo API",
        "license": {
            "url": "http://mit.com",
            "name": "MIT"
        },
        "description": "This is an API that tests servant-swagger support for a Todo API"
    },
    "paths": {
        "/todo/{id}": {
            "get": {
                "tags": [
                    "todo"
                ],
                "parameters": [
                    {
                        "required": true,
                        "schema": {
                            "type": "string"
                        },
                        "in": "path",
                        "name": "id",
                        "description": "TodoId param"
                    }
                ],
                "responses": {
                    "200": {
                        "content": {
                            "application/json": {
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
                                }
                            }
                        },
                        "description": "OK"
                    }
                }
            }
        }
    },
    "components": {}
}
|]

petstoreExampleJSON :: Value
petstoreExampleJSON = [aesonQQ|
{
  "openapi": "3.0.0",
  "info": {
    "version": "1.0.0",
    "title": "Swagger Petstore",
    "license": {
      "name": "MIT"
    }
  },
  "servers": [
    {
      "url": "http://petstore.swagger.io/v1"
    }
  ],
  "paths": {
    "/pets": {
      "get": {
        "summary": "List all pets",
        "operationId": "listPets",
        "tags": [
          "pets"
        ],
        "parameters": [
          {
            "name": "limit",
            "in": "query",
            "description": "How many items to return at one time (max 100)",
            "required": false,
            "schema": {
              "type": "integer",
              "format": "int32"
            }
          }
        ],
        "responses": {
          "200": {
            "description": "A paged array of pets",
            "headers": {
              "x-next": {
                "description": "A link to the next page of responses",
                "schema": {
                  "type": "string"
                }
              }
            },
            "content": {
              "application/json": {
                "schema": {
                  "type": "array",
                  "items": {
                    "type": "object",
                    "required": [
                      "id",
                      "name"
                    ],
                    "properties": {
                      "id": {
                        "type": "integer",
                        "format": "int64"
                      },
                      "name": {
                        "type": "string"
                      },
                      "tag": {
                        "type": "string"
                      }
                    }
                  }
                }
              }
            }
          },
          "default": {
            "description": "unexpected error",
            "content": {
              "application/json": {
                "schema": {
                  "type": "object",
                  "required": [
                    "code",
                    "message"
                  ],
                  "properties": {
                    "code": {
                      "type": "integer",
                      "format": "int32"
                    },
                    "message": {
                      "type": "string"
                    }
                  }
                }
              }
            }
          }
        }
      },
      "post": {
        "summary": "Create a pet",
        "operationId": "createPets",
        "tags": [
          "pets"
        ],
        "responses": {
          "201": {
            "description": "Null response"
          },
          "default": {
            "description": "unexpected error",
            "content": {
              "application/json": {
                "schema": {
                  "type": "object",
                  "required": [
                    "code",
                    "message"
                  ],
                  "properties": {
                    "code": {
                      "type": "integer",
                      "format": "int32"
                    },
                    "message": {
                      "type": "string"
                    }
                  }
                }
              }
            }
          }
        }
      }
    },
    "/pets/{petId}": {
      "get": {
        "summary": "Info for a specific pet",
        "operationId": "showPetById",
        "tags": [
          "pets"
        ],
        "parameters": [
          {
            "name": "petId",
            "in": "path",
            "required": true,
            "description": "The id of the pet to retrieve",
            "schema": {
              "type": "string"
            }
          }
        ],
        "responses": {
          "200": {
            "description": "Expected response to a valid request",
            "content": {
              "application/json": {
                "schema": {
                  "type": "object",
                  "required": [
                    "id",
                    "name"
                  ],
                  "properties": {
                    "id": {
                      "type": "integer",
                      "format": "int64"
                    },
                    "name": {
                      "type": "string"
                    },
                    "tag": {
                      "type": "string"
                    }
                  }
                }
              }
            }
          },
          "default": {
            "description": "unexpected error",
            "content": {
              "application/json": {
                "schema": {
                  "type": "object",
                  "required": [
                    "code",
                    "message"
                  ],
                  "properties": {
                    "code": {
                      "type": "integer",
                      "format": "int32"
                    },
                    "message": {
                      "type": "string"
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  },
  "components": {
    "schemas": {
      "Pet": {
        "type": "object",
        "required": [
          "id",
          "name"
        ],
        "properties": {
          "id": {
            "type": "integer",
            "format": "int64"
          },
          "name": {
            "type": "string"
          },
          "tag": {
            "type": "string"
          }
        }
      },
      "Pets": {
        "type": "array",
        "items": {
          "type": "object",
          "required": [
            "id",
            "name"
          ],
          "properties": {
            "id": {
              "type": "integer",
              "format": "int64"
            },
            "name": {
              "type": "string"
            },
            "tag": {
              "type": "string"
            }
          }
        }
      },
      "Error": {
        "type": "object",
        "required": [
          "code",
          "message"
        ],
        "properties": {
          "code": {
            "type": "integer",
            "format": "int32"
          },
          "message": {
            "type": "string"
          }
        }
      }
    }
  }
}
|]

compositionSchemaExample :: Schema
compositionSchemaExample = mempty
  & type_ ?~ SwaggerObject
  & Data.Swagger.allOf ?~ [
      Ref (Reference "Other")
    , Inline (mempty
             & type_ ?~ SwaggerObject
             & properties .~
                  [ ("greet", Inline $ mempty
                            & type_ ?~ SwaggerString) ])
  ]

compositionSchemaExampleJSON :: Value
compositionSchemaExampleJSON = [aesonQQ|
{
  "type": "object",
  "allOf": [
      {
         "$ref": "#/components/schemas/Other"
      },
      {
        "type": "object",
        "properties": {
          "greet": { "type": "string" }
        }
      }
  ]
}
|]
