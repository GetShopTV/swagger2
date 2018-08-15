{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
module Data.Swagger.Schema.GeneratorSpec where

import           Prelude                             ()
import           Prelude.Compat

import           Data.Swagger
import           Data.Swagger.Schema.Generator

import           Control.Lens.Operators
import           Data.Aeson
import           Data.Hashable                       (Hashable)
import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as HashMap
import           "unordered-containers" Data.HashSet (HashSet)
import qualified "unordered-containers" Data.HashSet as HashSet
import           Data.Int
import           Data.IntMap                         (IntMap)
import           Data.List.NonEmpty.Compat           (NonEmpty (..), nonEmpty)
import           Data.Map                            (Map, fromList)
import           Data.Monoid                         (mempty)
import           Data.Proxy
import           Data.Proxy
import           Data.Set                            (Set)
import qualified Data.Text                           as T
import qualified Data.Text.Lazy                      as TL
import           Data.Time
import           Data.Version                        (Version)
import           Data.Word
import           GHC.Generics

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

shouldValidate :: (FromJSON a, ToSchema a) => Proxy a -> Property
shouldValidate = validateFromJSON


shouldNotValidate :: (FromJSON a, ToSchema a) => Proxy a -> Property
shouldNotValidate = expectFailure . shouldValidate


spec :: Spec
spec = do
  describe "FromJSON validation" $ do
    prop "Bool" $ shouldValidate (Proxy :: Proxy Bool)
    prop "Char" $ shouldValidate (Proxy :: Proxy Char)
    prop "Double" $ shouldValidate (Proxy :: Proxy Double)
    prop "Float" $ shouldValidate (Proxy :: Proxy Float)
    prop "Int" $ shouldValidate (Proxy :: Proxy Int)
    prop "Int8" $ shouldValidate (Proxy :: Proxy Int8)
    prop "Int16" $ shouldValidate (Proxy :: Proxy Int16)
    prop "Int32" $ shouldValidate (Proxy :: Proxy Int32)
    prop "Int64" $ shouldValidate (Proxy :: Proxy Int64)
    prop "Integer" $ shouldValidate (Proxy :: Proxy Integer)
    prop "Word" $ shouldValidate (Proxy :: Proxy Word)
    prop "Word8" $ shouldValidate (Proxy :: Proxy Word8)
    prop "Word16" $ shouldValidate (Proxy :: Proxy Word16)
    prop "Word32" $ shouldValidate (Proxy :: Proxy Word32)
    prop "Word64" $ shouldValidate (Proxy :: Proxy Word64)
    prop "String" $ shouldValidate (Proxy :: Proxy String)
    prop "()" $ shouldValidate (Proxy :: Proxy ())
--    prop "ZonedTime" $ shouldValidate (Proxy :: Proxy ZonedTime)
--    prop "UTCTime" $ shouldValidate (Proxy :: Proxy UTCTime)
    prop "T.Text" $ shouldValidate (Proxy :: Proxy T.Text)
    prop "TL.Text" $ shouldValidate (Proxy :: Proxy TL.Text)
    prop "[String]" $ shouldValidate (Proxy :: Proxy [String])
    -- prop "(Maybe [Int])" $ shouldValidate (Proxy :: Proxy (Maybe [Int]))
    prop "(IntMap String)" $ shouldValidate (Proxy :: Proxy (IntMap String))
    prop "(Set Bool)" $ shouldValidate (Proxy :: Proxy (Set Bool))
    prop "(NonEmpty Bool)" $ shouldValidate (Proxy :: Proxy (NonEmpty Bool))
    prop "(HashSet Bool)" $ shouldValidate (Proxy :: Proxy (HashSet Bool))
    prop "(Either Int String)" $ shouldValidate (Proxy :: Proxy (Either Int String))
    prop "(Int, String)" $ shouldValidate (Proxy :: Proxy (Int, String))
    prop "(Map String Int)" $ shouldValidate (Proxy :: Proxy (Map String Int))
    prop "(Map T.Text Int)" $ shouldValidate (Proxy :: Proxy (Map T.Text Int))
    prop "(Map TL.Text Bool)" $ shouldValidate (Proxy :: Proxy (Map TL.Text Bool))
    prop "(HashMap String Int)" $ shouldValidate (Proxy :: Proxy (HashMap String Int))
    prop "(HashMap T.Text Int)" $ shouldValidate (Proxy :: Proxy (HashMap T.Text Int))
    prop "(HashMap TL.Text Bool)" $ shouldValidate (Proxy :: Proxy (HashMap TL.Text Bool))
    prop "Object" $ shouldValidate (Proxy :: Proxy Object)
    prop "(Int, String, Double)" $ shouldValidate (Proxy :: Proxy (Int, String, Double))
    prop "(Int, String, Double, [Int])" $ shouldValidate (Proxy :: Proxy (Int, String, Double, [Int]))
    prop "(Int, String, Double, [Int], Int)" $ shouldValidate (Proxy :: Proxy (Int, String, Double, [Int], Int))
  describe "Invalid FromJSON validation" $ do
    prop "WrongType" $ shouldNotValidate (Proxy :: Proxy WrongType)
    prop "MissingRequired" $ shouldNotValidate (Proxy :: Proxy MissingRequired)
    prop "MissingProperty" $ shouldNotValidate (Proxy :: Proxy MissingProperty)
    prop "WrongPropType" $ shouldNotValidate (Proxy :: Proxy WrongPropType)

-- =============================
-- Data types and bunk instances
-- =============================

data WrongType = WrongType Bool

instance FromJSON WrongType where
    parseJSON = withBool "WrongType" $ return . WrongType

instance ToSchema WrongType where
    declareNamedSchema _ = return . NamedSchema (Just "WrongType") $
                           mempty
                            & type_ .~ SwaggerObject


data MissingRequired = MissingRequired
    { propA :: String
    , propB :: Bool
    }

instance FromJSON MissingRequired where
    parseJSON = withObject "MissingRequired" $ \o ->
                  MissingRequired
                    <$> o .: "propA"
                    <*> o .: "propB"

instance ToSchema MissingRequired where
    declareNamedSchema _ = do
      stringSchema <- declareSchemaRef (Proxy :: Proxy String)
      boolSchema <- declareSchemaRef (Proxy :: Proxy Bool)
      return . NamedSchema (Just "MissingRequired") $
        mempty
        & type_ .~ SwaggerObject
        & properties .~ [("propA", stringSchema)
                        ,("propB", boolSchema)
                        ]
        & required .~ ["propA"]

data MissingProperty = MissingProperty
    { propC :: String
    , propD :: Bool
    }

instance FromJSON MissingProperty where
    parseJSON = withObject "MissingProperty" $ \o ->
                  MissingProperty
                    <$> o .: "propC"
                    <*> o .: "propD"

instance ToSchema MissingProperty where
    declareNamedSchema _ = do
      stringSchema <- declareSchemaRef (Proxy :: Proxy String)
      return . NamedSchema (Just "MissingProperty") $
        mempty
        & type_ .~ SwaggerObject
        & properties .~ [("propC", stringSchema)]
        & required .~ ["propC"]

data WrongPropType = WrongPropType
    { propE :: String
    }

instance FromJSON WrongPropType where
    parseJSON = withObject "WrongPropType" $ \o ->
                  WrongPropType
                    <$> o .: "propE"

instance ToSchema WrongPropType where
    declareNamedSchema _ = do
      boolSchema <- declareSchemaRef (Proxy :: Proxy Bool)
      return . NamedSchema (Just "WrongPropType") $
        mempty
        & type_ .~ SwaggerObject
        & properties .~ [("propE", boolSchema)]
        & required .~ ["propE"]
