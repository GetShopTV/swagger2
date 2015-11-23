{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.Swagger.SchemaSpec where

import Data.Aeson
import Data.Aeson.QQ
import Data.Proxy
import GHC.Generics

import Data.Swagger
import SpecCommon
import Test.Hspec

checkToSchema :: ToSchema a => Proxy a -> Value -> Spec
checkToSchema proxy js = toSchema proxy <~> js

spec :: Spec
spec = do
  describe "Generic ToSchema" $
    context "Person" $ checkToSchema (Proxy :: Proxy Person) personSchemaJSON

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
