{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.Swagger.SchemaSpec where

import Data.Aeson
import Data.Aeson.QQ
import Data.Char
import Data.Proxy
import GHC.Generics

import Data.Swagger
import Data.Swagger.Schema

import SpecCommon
import Test.Hspec

checkToSchema :: ToSchema a => Proxy a -> Value -> Spec
checkToSchema proxy js = toSchema proxy <~> js

spec :: Spec
spec = do
  describe "Generic ToSchema" $ do
    context "Person" $ checkToSchema (Proxy :: Proxy Person) personSchemaJSON
    context "ISPair" $ checkToSchema (Proxy :: Proxy ISPair) ispairSchemaJSON
    context "Point (custom fieldLabelModifier)" $ checkToSchema (Proxy :: Proxy Point) pointSchemaJSON

main :: IO ()
main = hspec spec

data Person = Person
  { name  :: String
  , phone :: Integer
  , email :: Maybe String
  } deriving (Generic, ToSchema)

personSchemaJSON :: Value
personSchemaJSON = [aesonQQ|
{
  "type": "object",
  "properties":
    {
      "name":   { "type": "string"  },
      "phone":  { "type": "integer" },
      "email":  { "type": "string"  }
    },
  "required": ["name", "phone"]
}
|]

data ISPair = ISPair Integer String
  deriving (Generic, ToSchema)

ispairSchemaJSON :: Value
ispairSchemaJSON = [aesonQQ|
{
  "type": "array",
  "items":
    [
      { "type": "integer" },
      { "type": "string"  }
    ]
}
|]

data Point = Point
  { pointX :: Double
  , pointY :: Double
  } deriving (Generic)

instance ToSchema Point where
  toSchema = genericToSchema defaultSchemaOptions
    { fieldLabelModifier = map toLower . drop (length "point") }

pointSchemaJSON :: Value
pointSchemaJSON = [aesonQQ|
{
  "type": "object",
  "properties":
    {
      "x": { "type": "number" },
      "y": { "type": "number" }
    },
  "required": ["x", "y"]
}
|]

