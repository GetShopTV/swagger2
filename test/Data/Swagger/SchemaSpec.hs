{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.Swagger.SchemaSpec where

import Prelude ()
import Prelude.Compat

import Control.Lens ((^.))
import Data.Aeson (Value, ToJSON(..), ToJSONKey(..))
import Data.Aeson.Types (toJSONKeyText)
import Data.Aeson.QQ
import Data.Char
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Proxy
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Text as Text
import GHC.Generics

import Data.Swagger
import Data.Swagger.Declare

import SpecCommon
import Test.Hspec

checkToSchema :: ToSchema a => Proxy a -> Value -> Spec
checkToSchema proxy js = toSchema proxy <=> js

checkSchemaName :: ToSchema a => Maybe String -> Proxy a -> Spec
checkSchemaName sname proxy =
  it ("schema name is " ++ show sname) $
    schemaName proxy `shouldBe` fmap Text.pack sname

checkDefs :: ToSchema a => Proxy a -> [String] -> Spec
checkDefs proxy names =
  it ("uses these definitions " ++ show names) $
    InsOrdHashMap.keys defs `shouldBe` map Text.pack names
  where
    defs = execDeclare (declareNamedSchema proxy) mempty

checkProperties :: ToSchema a => Proxy a -> [String] -> Spec
checkProperties proxy names =
  it ("has these fields in order " ++ show names) $
    InsOrdHashMap.keys fields `shouldBe` map Text.pack names
  where
    fields = toSchema proxy ^. properties

checkInlinedSchema :: ToSchema a => Proxy a -> Value -> Spec
checkInlinedSchema proxy js = toInlinedSchema proxy <=> js

checkInlinedSchemas :: ToSchema a => [String] -> Proxy a -> Value -> Spec
checkInlinedSchemas names proxy js = inlineSchemas (map Text.pack names) defs s <=> js
  where
    (defs, s) = runDeclare (declareSchema proxy) mempty

checkInlinedRecSchema :: ToSchema a => Proxy a -> Value -> Spec
checkInlinedRecSchema proxy js = inlineNonRecursiveSchemas defs s <=> js
  where
    (defs, s) = runDeclare (declareSchema proxy) mempty

spec :: Spec
spec = do
  describe "Generic ToSchema" $ do
    context "Unit" $ checkToSchema (Proxy :: Proxy Unit) unitSchemaJSON
    context "Person" $ checkToSchema (Proxy :: Proxy Person) personSchemaJSON
    context "ISPair" $ checkToSchema (Proxy :: Proxy ISPair) ispairSchemaJSON
    context "Point (fieldLabelModifier)" $ checkToSchema (Proxy :: Proxy Point) pointSchemaJSON
    context "Point5 (many field record)" $ do
      checkToSchema (Proxy :: Proxy Point5) point5SchemaJSON
      checkProperties (Proxy :: Proxy Point5) point5Properties
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
    context "MyRoseTree' (datatypeNameModifier)" $ checkToSchema (Proxy :: Proxy MyRoseTree') myRoseTreeSchemaJSON'
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
    context "MyRoseTree'" $ checkDefs (Proxy :: Proxy MyRoseTree') ["myrosetree'"]
    context "[Set (Unit, Maybe Color)]" $ checkDefs (Proxy :: Proxy [Set (Unit, Maybe Color)]) ["Unit", "Color"]
    context "ResourceId" $ checkDefs (Proxy :: Proxy ResourceId) []
  describe "Inlining Schemas" $ do
    context "Paint" $ checkInlinedSchema (Proxy :: Proxy Paint) paintInlinedSchemaJSON
    context "Character" $ checkInlinedSchema (Proxy :: Proxy Character) characterInlinedSchemaJSON
    context "Character (inlining only Player)" $ checkInlinedSchemas ["Player"] (Proxy :: Proxy Character) characterInlinedPlayerSchemaJSON
    context "Light" $ checkInlinedSchema (Proxy :: Proxy Light) lightInlinedSchemaJSON
    context "MyRoseTree (inlineNonRecursiveSchemas)" $ checkInlinedRecSchema (Proxy :: Proxy MyRoseTree) myRoseTreeSchemaJSON
    context "MyRoseTree' (inlineNonRecursiveSchemas)" $ checkInlinedRecSchema (Proxy :: Proxy MyRoseTree') myRoseTreeSchemaJSON'
  describe "Bounded Enum key mapping" $ do
    context "ButtonImages" $ checkToSchema (Proxy :: Proxy ButtonImages) buttonImagesSchemaJSON

main :: IO ()
main = hspec spec

-- ========================================================================
-- Person (simple record with optional fields)
-- ========================================================================
data Person = Person
  { name  :: String
  , phone :: Integer
  , email :: Maybe String
  } deriving (Generic)

instance ToSchema Person

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
  deriving (Generic)

instance ToSchema ISPair

ispairSchemaJSON :: Value
ispairSchemaJSON = [aesonQQ|
{
  "type": "array",
  "items":
    [
      { "type": "integer" },
      { "type": "string"  }
    ],
  "minItems": 2,
  "maxItems": 2
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
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions
    { fieldLabelModifier = map toLower . drop (length "point") }

pointSchemaJSON :: Value
pointSchemaJSON = [aesonQQ|
{
  "type": "object",
  "properties":
    {
      "x": { "type": "number", "format": "double" },
      "y": { "type": "number", "format": "double" }
    },
  "required": ["x", "y"]
}
|]

-- ========================================================================
-- Point (record data type with multiple fields)
-- ========================================================================

data Point5 = Point5
  { point5X :: Double
  , point5Y :: Double
  , point5Z :: Double
  , point5U :: Double
  , point5V :: Double -- 5 dimensional!
  } deriving (Generic)

instance ToSchema Point5 where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions
    { fieldLabelModifier = map toLower . drop (length "point5") }

point5SchemaJSON :: Value
point5SchemaJSON = [aesonQQ|
{
  "type": "object",
  "properties":
    {
      "x": { "type": "number", "format": "double" },
      "y": { "type": "number", "format": "double" },
      "z": { "type": "number", "format": "double" },
      "u": { "type": "number", "format": "double" },
      "v": { "type": "number", "format": "double" }
    },
  "required": ["x", "y", "z", "u", "v"]
}
|]

point5Properties :: [String]
point5Properties = ["x", "y", "z", "u", "v"]

-- ========================================================================
-- Color (enum)
-- ========================================================================
data Color
  = Red
  | Green
  | Blue
  deriving (Generic)
instance ToSchema Color

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

data Shade = Dim | Bright deriving (Generic)
instance ToParamSchema Shade

instance ToSchema Shade where declareNamedSchema = pure . paramSchemaToNamedSchema defaultSchemaOptions

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
  deriving (Generic)
instance ToSchema Paint

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

paintInlinedSchemaJSON :: Value
paintInlinedSchemaJSON = [aesonQQ|
{
  "type": "object",
  "properties":
    {
      "color":
        {
          "type": "string",
          "enum": ["Red", "Green", "Blue"]
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
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions
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
  deriving (Eq, Ord, Generic)
instance ToSchema UserId

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
  deriving (Generic)
instance ToSchema UserGroup

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
  } deriving (Generic)
instance ToSchema Player

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
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions
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

data MyRoseTree' = MyRoseTree'
  { root'  :: String
  , trees' :: [MyRoseTree']
  } deriving (Generic)

instance ToSchema MyRoseTree' where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions
    { datatypeNameModifier = map toLower }

myRoseTreeSchemaJSON' :: Value
myRoseTreeSchemaJSON' = [aesonQQ|
{
  "type": "object",
  "properties":
    {
      "root'": { "type": "string" },
      "trees'":
        {
          "type": "array",
          "items":
            {
              "$ref": "#/definitions/myrosetree'"
            }
        }
    },
  "required": ["root'", "trees'"]
}
|]

-- ========================================================================
-- Inlined (newtype for inlining schemas)
-- ========================================================================

newtype Inlined a = Inlined { getInlined :: a }

instance ToSchema a => ToSchema (Inlined a) where
  declareNamedSchema _ = unname <$> declareNamedSchema (Proxy :: Proxy a)
    where
      unname (NamedSchema _ s) = NamedSchema Nothing s

newtype Players = Players [Inlined Player]
  deriving (Generic)
instance ToSchema Players

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
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions
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

data Unit = Unit deriving (Generic)
instance ToSchema Unit

unitSchemaJSON :: Value
unitSchemaJSON = [aesonQQ|
{
  "type": "string",
  "enum": ["Unit"]
}
|]


-- ========================================================================
-- Character (sum type with ref and record in alternative)
-- ========================================================================

data Character
  = PC Player
  | NPC { npcName :: String, npcPosition :: Point }
  deriving (Generic)
instance ToSchema Character

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

characterInlinedSchemaJSON :: Value
characterInlinedSchemaJSON = [aesonQQ|
{
  "type": "object",
  "properties":
    {
      "PC":
        {
          "type": "object",
          "properties":
            {
              "position":
                {
                  "type": "object",
                  "properties":
                    {
                      "x": { "type": "number", "format": "double" },
                      "y": { "type": "number", "format": "double" }
                    },
                  "required": ["x", "y"]
                }
            },
          "required": ["position"]
        },
      "NPC":
        {
          "type": "object",
          "properties":
            {
              "npcName": { "type": "string" },
              "npcPosition":
                {
                  "type": "object",
                  "properties":
                    {
                      "x": { "type": "number", "format": "double" },
                      "y": { "type": "number", "format": "double" }
                    },
                  "required": ["x", "y"]
                }
            },
          "required": ["npcName", "npcPosition"]
        }
    },
  "maxProperties": 1,
  "minProperties": 1
}
|]

characterInlinedPlayerSchemaJSON :: Value
characterInlinedPlayerSchemaJSON = [aesonQQ|
{
  "type": "object",
  "properties":
    {
      "PC":
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
        },
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
  = NoLight
  | LightFreq Double
  | LightColor Color
  | LightWaveLength { waveLength :: Double }
  deriving (Generic)

instance ToSchema Light where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
    { unwrapUnaryRecords = True }

lightSchemaJSON :: Value
lightSchemaJSON = [aesonQQ|
{
  "type": "object",
  "properties":
    {
      "NoLight": { "type": "array", "items": [] },
      "LightFreq": { "type": "number", "format": "double" },
      "LightColor": { "$ref": "#/definitions/Color" },
      "LightWaveLength": { "type": "number", "format": "double" }
    },
  "maxProperties": 1,
  "minProperties": 1
}
|]

lightInlinedSchemaJSON :: Value
lightInlinedSchemaJSON = [aesonQQ|
{
  "type": "object",
  "properties":
    {
      "NoLight": { "type": "array", "items": [] },
      "LightFreq": { "type": "number", "format": "double" },
      "LightColor":
        {
          "type": "string",
          "enum": ["Red", "Green", "Blue"]
        },
      "LightWaveLength": { "type": "number", "format": "double" }
    },
  "maxProperties": 1,
  "minProperties": 1
}
|]

-- ========================================================================
-- ResourceId (series of newtypes)
-- ========================================================================

newtype Id = Id String deriving (Generic)
instance ToSchema Id

newtype ResourceId = ResourceId Id deriving (Generic)
instance ToSchema ResourceId

-- ========================================================================
-- ButtonImages (bounded enum key mapping)
-- ========================================================================

data ButtonState = Neutral | Focus | Active | Hover | Disabled
  deriving (Show, Bounded, Enum, Generic)

instance ToJSON ButtonState
instance ToSchema ButtonState
instance ToJSONKey ButtonState where toJSONKey = toJSONKeyText (Text.pack . show)

type ImageUrl = Text.Text

newtype ButtonImages = ButtonImages { getButtonImages :: Map ButtonState ImageUrl }
  deriving (Generic)

instance ToJSON ButtonImages where
  toJSON = toJSON . getButtonImages

instance ToSchema ButtonImages where
  declareNamedSchema = genericDeclareNamedSchemaNewtype defaultSchemaOptions
    declareSchemaBoundedEnumKeyMapping

buttonImagesSchemaJSON :: Value
buttonImagesSchemaJSON = [aesonQQ|
{
  "type": "object",
  "properties":
    {
      "Neutral":  { "type": "string" },
      "Focus":    { "type": "string" },
      "Active":   { "type": "string" },
      "Hover":    { "type": "string" },
      "Disabled": { "type": "string" }
    }
}
|]

