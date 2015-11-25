{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.Swagger.SchemaSpec where

import Data.Aeson
import Data.Aeson.QQ
import Data.Char
import Data.Proxy
import Data.Set (Set)
import GHC.Generics

import Data.Swagger
import Data.Swagger.Schema

import SpecCommon
import Test.Hspec

checkToSchema :: ToSchema a => Proxy a -> Value -> Spec
checkToSchema proxy js = toSchema proxy <~> js

checkSchemaName :: ToSchema a => Maybe String -> Proxy a -> Spec
checkSchemaName name proxy =
  it ("schema name is " ++ show name) $
    schemaName proxy `shouldBe` name

spec :: Spec
spec = do
  describe "Generic ToSchema" $ do
    context "Person" $ checkToSchema (Proxy :: Proxy Person) personSchemaJSON
    context "ISPair" $ checkToSchema (Proxy :: Proxy ISPair) ispairSchemaJSON
    context "Point (fieldLabelModifier)" $ checkToSchema (Proxy :: Proxy Point) pointSchemaJSON
    context "Color (bounded enum)" $ checkToSchema (Proxy :: Proxy Color) colorSchemaJSON
    context "Paint (record with bounded enum field)" $ checkToSchema (Proxy :: Proxy Paint) paintSchemaJSON
    context "UserGroup (set newtype)" $ checkToSchema (Proxy :: Proxy UserGroup) userGroupSchemaJSON
    context "Unary records" $ do
      context "Email (unwrapUnaryRecords)"  $ checkToSchema (Proxy :: Proxy Email)  emailSchemaJSON
      context "UserId (non-record newtype)" $ checkToSchema (Proxy :: Proxy UserId) userIdSchemaJSON
      context "Player (unary record)" $ checkToSchema (Proxy :: Proxy Player) playerSchemaJSON
    context "Players (inlining schema)" $ checkToSchema (Proxy :: Proxy Players) playersSchemaJSON
    context "MyRoseTree (datatypeNameModifier)" $ checkToSchema (Proxy :: Proxy MyRoseTree) myRoseTreeSchemaJSON
    context "Sum types" $ do
      context "Status (sum of unary constructors)" $ checkToSchema (Proxy :: Proxy Status) statusSchemaJSON
    context "Schema name" $ do
      context "String" $ checkSchemaName Nothing (Proxy :: Proxy String)
      context "(Int, Float)" $ checkSchemaName Nothing (Proxy :: Proxy (Int, Float))
      context "Person" $ checkSchemaName (Just "Person") (Proxy :: Proxy Person)
  describe "toSchemaBoundedEnum" $ do
    context "Color" $ checkToSchema (Proxy :: Proxy Color) colorSchemaJSON

main :: IO ()
main = hspec spec

-- ========================================================================
-- Person (simple record with optional fields)
-- ========================================================================
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

-- ========================================================================
-- ISPair (non-record product data type)
-- ========================================================================
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

-- ========================================================================
-- Point (record data type with custom fieldLabelModifier)
-- ========================================================================
data Point = Point
  { pointX :: Double
  , pointY :: Double
  } deriving (Generic)

instance ToSchema Point where
  toNamedSchema = genericToNamedSchema defaultSchemaOptions
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


-- ========================================================================
-- Color (bounded enum)
-- ========================================================================
data Color
  = Red
  | Green
  | Blue
  deriving (Generic, Enum, Bounded, ToJSON)

instance ToSchema Color where
  toNamedSchema = genericToNamedSchemaBoundedEnum defaultSchemaOptions

colorSchemaJSON :: Value
colorSchemaJSON = [aesonQQ|
{
  "type": "string",
  "enum": ["Red", "Green", "Blue"]
}
|]

-- ========================================================================
-- Paint (record with bounded enum property)
-- ========================================================================

newtype Paint = Paint { color :: Color }
  deriving (Generic, ToSchema)

paintSchemaJSON :: Value
paintSchemaJSON = [aesonQQ|
{
  "type": "object",
  "properties":
    {
      "color":
        {
          "$ref": "#/definitions/Color"
        }
    },
  "required": ["color"]
}
|]

-- ========================================================================
-- Email (newtype with unwrapUnaryRecords set to True)
-- ========================================================================

newtype Email = Email { getEmail :: String }
  deriving (Generic)

instance ToSchema Email where
  toNamedSchema = genericToNamedSchema defaultSchemaOptions
    { unwrapUnaryRecords = True }

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
  deriving (Eq, Ord, Generic, ToSchema)

userIdSchemaJSON :: Value
userIdSchemaJSON = [aesonQQ|
{
  "type": "integer"
}
|]

-- ========================================================================
-- UserGroup (set newtype)
-- ========================================================================

newtype UserGroup = UserGroup (Set UserId)
  deriving (Generic, ToSchema)

userGroupSchemaJSON :: Value
userGroupSchemaJSON = [aesonQQ|
{
  "type": "array",
  "items": { "$ref": "#/definitions/UserId" },
  "uniqueItems": true
}
|]

-- ========================================================================
-- Player (record newtype)
-- ========================================================================

newtype Player = Player
  { position :: Point
  } deriving (Generic, ToSchema)

playerSchemaJSON :: Value
playerSchemaJSON = [aesonQQ|
{
  "type": "object",
  "properties":
    {
      "position":
        {
          "$ref": "#/definitions/Point"
        }
    },
  "required": ["position"]
}
|]

-- ========================================================================
-- MyRoseTree (custom datatypeNameModifier)
-- ========================================================================

data MyRoseTree = MyRoseTree
  { root  :: String
  , trees :: [MyRoseTree]
  } deriving (Generic)

instance ToSchema MyRoseTree where
  toNamedSchema = genericToNamedSchema defaultSchemaOptions
    { datatypeNameModifier = drop (length "My") }

myRoseTreeSchemaJSON :: Value
myRoseTreeSchemaJSON = [aesonQQ|
{
  "type": "object",
  "properties":
    {
      "root": { "type": "string" },
      "trees":
        {
          "type": "array",
          "items":
            {
              "$ref": "#/definitions/RoseTree"
            }
        }
    },
  "required": ["root", "trees"]
}
|]

-- ========================================================================
-- Inlined (newtype for inlining schemas)
-- ========================================================================

newtype Inlined a = Inlined { getInlined :: a }

instance ToSchema a => ToSchema (Inlined a) where
  toNamedSchema _ = (Nothing, toSchema (Proxy :: Proxy a))

newtype Players = Players [Inlined Player]
  deriving (Generic, ToSchema)

playersSchemaJSON :: Value
playersSchemaJSON = [aesonQQ|
{
  "type": "array",
  "items":
    {
      "type": "object",
      "properties":
        {
          "position":
            {
              "$ref": "#/definitions/Point"
            }
        },
      "required": ["position"]
    }
}
|]

-- ========================================================================
-- Status (sum type with unary constructors)
-- ========================================================================

data Status
  = StatusOk String
  | StatusError String
  deriving (Generic, ToSchema)

statusSchemaJSON :: Value
statusSchemaJSON = [aesonQQ|
{
  "type": "object",
  "properties":
    {
      "StatusOk": { "type": "string" },
      "StatusError": { "type": "string" }
    },
  "maxProperties": 1,
  "minProperties": 1
}
|]

