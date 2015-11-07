{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.SwaggerSpec where

import Data.Aeson
import Data.Aeson.QQ
import Data.Swagger

import Test.Hspec
import Test.Hspec.HUnit

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
