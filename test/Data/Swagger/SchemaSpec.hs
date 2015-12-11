{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.Swagger.SchemaSpec where

import Data.Aeson
import Data.Aeson.QQ
import Data.Char
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics

import Data.Swagger
import Data.Swagger.Declare

import SpecCommon
import Test.Hspec

checkToSchema :: ToSchema a => Proxy a -> Value -> Spec
checkToSchema proxy js = toSchema proxy <~> js

checkSchemaName :: ToSchema a => Maybe String -> Proxy a -> Spec
checkSchemaName sname proxy =
  it ("schema name is " ++ show sname) $
    schemaName proxy `shouldBe` sname

checkDefs :: ToSchema a => Proxy a -> [String] -> Spec
checkDefs proxy names =
  it ("uses these definitions " ++ show names) $
    Set.fromList (HashMap.keys defs) `shouldBe` Set.fromList (map Text.pack names)
  where
    defs = execDeclare (toSchemaDefinitions proxy) mempty

spec :: Spec
spec = do
  describe "Generic ToSchema" $ do
    context "Unit" $ checkToSchema (Proxy :: Proxy Unit) unitSchemaJSON
    context "Person" $ checkToSchema (Proxy :: Proxy Person) personSchemaJSON
    context "ISPair" $ checkToSchema (Proxy :: Proxy ISPair) ispairSchemaJSON
    context "Point (fieldLabelModifier)" $ checkToSchema (Proxy :: Proxy Point) pointSchemaJSON
    context "Color (bounded enum)" $ checkToSchema (Proxy :: Proxy Color) colorSchemaJSON
    context "Shade (paramSchemaToNamedSchema)" $ checkToSchema (Proxy :: Proxy Shade) shadeSchemaJSON
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
      context "Character (ref and record sum)" $ checkToSchema (Proxy :: Proxy Character) characterSchemaJSON
      context "Light (sum with unwrapUnaryRecords)" $ checkToSchema (Proxy :: Proxy Light) lightSchemaJSON
    context "Schema name" $ do
      context "String" $ checkSchemaName Nothing (Proxy :: Proxy String)
      context "(Int, Float)" $ checkSchemaName Nothing (Proxy :: Proxy (Int, Float))
      context "Person" $ checkSchemaName (Just "Person") (Proxy :: Proxy Person)
      context "Shade" $ checkSchemaName (Just "Shade") (Proxy :: Proxy Shade)
  describe "Generic Definitions" $ do
    context "Unit" $ checkDefs (Proxy :: Proxy Unit) []
    context "Paint" $ checkDefs (Proxy :: Proxy Paint) ["Color"]
    context "Light" $ checkDefs (Proxy :: Proxy Light) ["Color"]
    context "Character" $ checkDefs (Proxy :: Proxy Character) ["Player", "Point"]
    context "MyRoseTree" $ checkDefs (Proxy :: Proxy MyRoseTree) ["RoseTree"]
    context "[Set (Unit, Maybe Color)]" $ checkDefs (Proxy :: Proxy [Set (Unit, Maybe Color)]) ["Unit", "Color"]

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
  toSchemaDefinitions = genericToSchemaDefinitions defaultSchemaOptions
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
-- Color (enum)
-- ========================================================================
data Color
  = Red
  | Green
  | Blue
  deriving (Generic, ToSchema)

colorSchemaJSON :: Value
colorSchemaJSON = [aesonQQ|
{
  "type": "string",
  "enum": ["Red", "Green", "Blue"]
}
|]

-- ========================================================================
-- Shade (paramSchemaToNamedSchema)
-- ========================================================================

data Shade = Dim | Bright deriving (Generic, ToParamSchema)

instance ToSchema Shade where toSchemaDefinitions = pure . paramSchemaToNamedSchema defaultSchemaOptions

shadeSchemaJSON :: Value
shadeSchemaJSON = [aesonQQ|
{
  "type": "string",
  "enum": ["Dim", "Bright"]
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
  toSchemaDefinitions = genericToSchemaDefinitions defaultSchemaOptions
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
  toSchemaDefinitions = genericToSchemaDefinitions defaultSchemaOptions
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
  toSchemaDefinitions _ = unname <$> toSchemaDefinitions (Proxy :: Proxy a)
    where
      unname (_, schema) = (Nothing, schema)

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
  deriving (Generic)

instance ToSchema Status where
  toSchemaDefinitions = genericToSchemaDefinitions defaultSchemaOptions
    { constructorTagModifier = map toLower . drop (length "Status") }

statusSchemaJSON :: Value
statusSchemaJSON = [aesonQQ|
{
  "type": "object",
  "properties":
    {
      "ok":    { "type": "string" },
      "error": { "type": "string" }
    },
  "maxProperties": 1,
  "minProperties": 1
}
|]

-- ========================================================================
-- Unit type
-- ========================================================================

data Unit = Unit deriving (Generic, ToSchema)

unitSchemaJSON :: Value
unitSchemaJSON = [aesonQQ|
{
  "type": "array",
  "enum": [[]]
}
|]


-- ========================================================================
-- Character (sum type with ref and record in alternative)
-- ========================================================================

data Character
  = PC Player
  | NPC { npcName :: String, npcPosition :: Point }
  deriving (Generic, ToSchema)

characterSchemaJSON :: Value
characterSchemaJSON = [aesonQQ|
{
  "type": "object",
  "properties":
    {
      "PC": { "$ref": "#/definitions/Player" },
      "NPC":
        {
          "type": "object",
          "properties":
            {
              "npcName": { "type": "string" },
              "npcPosition": { "$ref": "#/definitions/Point" }
            },
          "required": ["npcName", "npcPosition"]
        }
    },
  "maxProperties": 1,
  "minProperties": 1
}
|]

-- ========================================================================
-- Light (sum type with unwrapUnaryRecords)
-- ========================================================================

data Light
  = LightFreq Double
  | LightColor Color
  | LightWaveLength { waveLength :: Double }
  deriving (Generic)

instance ToSchema Light where
  toSchemaDefinitions = genericToSchemaDefinitions defaultSchemaOptions
    { unwrapUnaryRecords = True }

lightSchemaJSON :: Value
lightSchemaJSON = [aesonQQ|
{
  "type": "object",
  "properties":
    {
      "LightFreq": { "type": "number" },
      "LightColor": { "$ref": "#/definitions/Color" },
      "LightWaveLength": { "type": "number" }
    },
  "maxProperties": 1,
  "minProperties": 1
}
|]

