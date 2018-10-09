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
import Data.Aeson.QQ.Simple
import Data.Char
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Proxy
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Text as Text
import GHC.Generics

import Data.Swagger
import Data.Swagger.Declare

import Data.Swagger.CommonTestTypes
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
      context "SingleMaybeField (unary record with Maybe)" $ checkToSchema (Proxy :: Proxy SingleMaybeField) singleMaybeFieldSchemaJSON
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
