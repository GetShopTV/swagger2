{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.SwaggerSpec where

import Data.Aeson
import Data.Aeson.QQ
import Data.Swagger

import Test.Hspec
import Test.Hspec.HUnit

infoExample :: SwaggerInfo
infoExample = SwaggerInfo
  { swaggerInfoTitle = "Swagger Sample App"
  , swaggerInfoDescription = Just "This is a sample server Petstore server."
  , swaggerInfoTermsOfService = Just "http://swagger.io/terms/"
  , swaggerInfoContact = Just contact
  , swaggerInfoLicense = Just license
  , swaggerInfoVersion = "1.0.1" }
  where
    contact = SwaggerContact
      { swaggerContactName = Just "API Support"
      , swaggerContactUrl = Just (URL "http://www.swagger.io/support")
      , swaggerContactEmail = Just "support@swagger.io" }
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

spec :: Spec
spec = do
  describe "Info Object" $ do
    it "encodes correctly" $ do
      toJSON infoExample `shouldBe` infoExampleJSON

main :: IO ()
main = hspec spec

