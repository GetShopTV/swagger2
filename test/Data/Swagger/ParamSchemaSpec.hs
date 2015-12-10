{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.Swagger.ParamSchemaSpec where

import Data.Aeson
import Data.Aeson.QQ
import Data.Char
import Data.Proxy
import Data.Set (Set)
import GHC.Generics

import Data.Swagger

import SpecCommon
import Test.Hspec

checkToParamSchema :: ToParamSchema a => Proxy a -> Value -> Spec
checkToParamSchema proxy js = (toParamSchema proxy :: ParamSchema Param Items) <=> js

spec :: Spec
spec = do
  describe "Generic ToParamSchema" $ do
    context "Color (bounded enum)" $ checkToParamSchema (Proxy :: Proxy Color) colorSchemaJSON
    context "Status (constructorTagModifier)" $ checkToParamSchema (Proxy :: Proxy Status) statusSchemaJSON
    context "Unary records" $ do
      context "Email (unary record)"  $ checkToParamSchema (Proxy :: Proxy Email)  emailSchemaJSON
      context "UserId (non-record newtype)" $ checkToParamSchema (Proxy :: Proxy UserId) userIdSchemaJSON

main :: IO ()
main = hspec spec

-- ========================================================================
-- Color (enum)
-- ========================================================================
data Color
  = Red
  | Green
  | Blue
  deriving (Generic, ToParamSchema)

colorSchemaJSON :: Value
colorSchemaJSON = [aesonQQ|
{
  "type": "string",
  "enum": ["Red", "Green", "Blue"]
}
|]

-- ========================================================================
-- Status (constructorTagModifier)
-- ========================================================================

data Status = StatusOk | StatusError deriving (Generic)

instance ToParamSchema Status where
  toParamSchema = genericToParamSchema defaultSchemaOptions
    { constructorTagModifier = map toLower . drop (length "Status") }

statusSchemaJSON :: Value
statusSchemaJSON = [aesonQQ|
{
  "type": "string",
  "enum": ["ok", "error"]
}
|]

-- ========================================================================
-- Email (newtype with unwrapUnaryRecords set to True)
-- ========================================================================

newtype Email = Email { getEmail :: String }
  deriving (Generic, ToParamSchema)

emailSchemaJSON :: Value
emailSchemaJSON = [aesonQQ|
{
  "type": "string"
}
|]

-- ========================================================================
-- UserId (non-record newtype)
-- ========================================================================

newtype UserId = UserId Integer
  deriving (Generic, ToParamSchema)

userIdSchemaJSON :: Value
userIdSchemaJSON = [aesonQQ|
{
  "type": "integer"
}
|]

