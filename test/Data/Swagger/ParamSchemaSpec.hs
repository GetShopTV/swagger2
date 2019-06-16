{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Swagger.ParamSchemaSpec where

import Data.Aeson
import Data.Aeson.QQ.Simple
import Data.Char
import Data.Proxy
import GHC.Generics

import Data.Swagger
import Data.Swagger.Internal (SwaggerKind(..))

import Data.Swagger.CommonTestTypes
import SpecCommon
import Test.Hspec
import Data.Time.LocalTime

import qualified Data.HashMap.Strict as HM

checkToParamSchema :: ToParamSchema a => Proxy a -> Value -> Spec
checkToParamSchema proxy js = (toParamSchema proxy :: ParamSchema ('SwaggerKindNormal Param)) <=> js

spec :: Spec
spec = do
  describe "Generic ToParamSchema" $ do
    context "Unit" $ checkToParamSchema (Proxy :: Proxy Unit) unitSchemaJSON
    context "Color (bounded enum)" $ checkToParamSchema (Proxy :: Proxy Color) colorSchemaJSON
    context "Status (constructorTagModifier)" $ checkToParamSchema (Proxy :: Proxy Status) statusSchemaJSON
    context "Unary records" $ do
      context "Email (unary record)"  $ checkToParamSchema (Proxy :: Proxy Email)  emailSchemaJSON
      context "UserId (non-record newtype)" $ checkToParamSchema (Proxy :: Proxy UserId) userIdSchemaJSON
    context "TimeOfDay" $ checkToParamSchema (Proxy :: Proxy TimeOfDay) (Object (HM.fromList [("format",String "hh:MM:ss"),("type",String "string")]))

main :: IO ()
main = hspec spec
