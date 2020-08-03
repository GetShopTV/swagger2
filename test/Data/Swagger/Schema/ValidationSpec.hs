{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Swagger.Schema.ValidationSpec where

import           Control.Applicative
import           Control.Lens                        ((&), (.~), (?~))
import           Data.Aeson
import           Data.Aeson.Types
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
import           Data.Set                            (Set)
import qualified Data.Text                           as T
import qualified Data.Text.Lazy                      as TL
import           Data.Time
import           Data.Version                        (Version)
import           Data.Word
import           GHC.Generics

import           Data.Swagger
import           Data.Swagger.Declare

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances           ()

shouldValidate :: (ToJSON a, ToSchema a) => Proxy a -> a -> Bool
shouldValidate _ x = validateToJSON x == []

shouldNotValidate :: forall a. ToSchema a => (a -> Value) -> a -> Bool
shouldNotValidate f = not . null . validateJSON defs sch . f
  where
    (defs, sch) = runDeclare (declareSchema (Proxy :: Proxy a)) mempty

spec :: Spec
spec = do
  describe "Validation" $ do
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
    prop "ZonedTime" $ shouldValidate (Proxy :: Proxy ZonedTime)
    prop "UTCTime" $ shouldValidate (Proxy :: Proxy UTCTime)
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
    prop "Person" $ shouldValidate (Proxy :: Proxy Person)
    prop "Color" $ shouldValidate (Proxy :: Proxy Color)
    prop "Paint" $ shouldValidate (Proxy :: Proxy Paint)
    prop "MyRoseTree" $ shouldValidate (Proxy :: Proxy MyRoseTree)
    prop "Light" $ shouldValidate (Proxy :: Proxy Light)
    prop "Light TaggedObject" $ shouldValidate (Proxy :: Proxy LightTaggedObject)
    prop "Light UntaggedValue" $ shouldValidate (Proxy :: Proxy LightUntaggedValue)
    prop "ButtonImages" $ shouldValidate (Proxy :: Proxy ButtonImages)
    prop "Version" $ shouldValidate (Proxy :: Proxy Version)
    prop "FreeForm" $ shouldValidate (Proxy :: Proxy FreeForm)

  describe "invalid cases" $ do
    prop "invalidPersonToJSON"        $ shouldNotValidate invalidPersonToJSON
    prop "invalidColorToJSON"         $ shouldNotValidate invalidColorToJSON
    prop "invalidPaintToJSON"         $ shouldNotValidate invalidPaintToJSON
    prop "invalidLightToJSON"         $ shouldNotValidate invalidLightToJSON
    prop "invalidButtonImagesToJSON"  $ shouldNotValidate invalidButtonImagesToJSON

main :: IO ()
main = hspec spec

-- ========================================================================
-- Person (simple record with optional fields)
-- ========================================================================
data Person = Person
  { name  :: String
  , phone :: Integer
  , email :: Maybe String
  } deriving (Show, Generic)

instance ToJSON Person
instance ToSchema Person

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary <*> arbitrary

invalidPersonToJSON :: Person -> Value
invalidPersonToJSON Person{..} = object
  [ T.pack "personName"  .= toJSON name
  , T.pack "personPhone" .= toJSON phone
  , T.pack "personEmail" .= toJSON email
  ]

-- ========================================================================
-- Color (enum)
-- ========================================================================
data Color = Red | Green | Blue deriving (Show, Generic, Bounded, Enum)

instance ToJSON Color
instance ToSchema Color

instance Arbitrary Color where
  arbitrary = arbitraryBoundedEnum

invalidColorToJSON :: Color -> Value
invalidColorToJSON Red   = toJSON "red"
invalidColorToJSON Green = toJSON "green"
invalidColorToJSON Blue  = toJSON "blue"

-- ========================================================================
-- Paint (record with bounded enum property)
-- ========================================================================

newtype Paint = Paint { color :: Color }
  deriving (Show, Generic)

instance ToJSON Paint
instance ToSchema Paint

instance Arbitrary Paint where
  arbitrary = Paint <$> arbitrary

invalidPaintToJSON :: Paint -> Value
invalidPaintToJSON = toJSON . color

-- ========================================================================
-- MyRoseTree (custom datatypeNameModifier)
-- ========================================================================

data MyRoseTree = MyRoseTree
  { root  :: String
  , trees :: [MyRoseTree]
  } deriving (Show, Generic)

instance ToJSON MyRoseTree

instance ToSchema MyRoseTree where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions
    { datatypeNameModifier = drop (length "My") }

instance Arbitrary MyRoseTree where
  arbitrary = fmap (cut limit) $ MyRoseTree <$> arbitrary <*> (take limit <$> arbitrary)
    where
      limit = 4
      cut 0 (MyRoseTree x _ ) = MyRoseTree x []
      cut n (MyRoseTree x xs) = MyRoseTree x (map (cut (n - 1)) xs)

-- ========================================================================
-- Light (sum type)
-- ========================================================================

data Light = NoLight | LightFreq Double | LightColor Color deriving (Show, Generic)

instance ToSchema Light where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions { Data.Swagger.sumEncoding = ObjectWithSingleField }

instance ToJSON Light where
  toJSON = genericToJSON defaultOptions { Data.Aeson.Types.sumEncoding = ObjectWithSingleField }

instance Arbitrary Light where
  arbitrary = oneof
    [ return NoLight
    , LightFreq <$> arbitrary
    , LightColor <$> arbitrary
    ]

invalidLightToJSON :: Light -> Value
invalidLightToJSON = genericToJSON defaultOptions

-- Check all SumEncoding flavors.

newtype LightTaggedObject = LightTaggedObject Light
  deriving (Show)

instance ToJSON LightTaggedObject where
  toJSON (LightTaggedObject light) = genericToJSON defaultOptions { Data.Aeson.Types.sumEncoding = defaultTaggedObject } light

instance ToSchema LightTaggedObject where
  declareNamedSchema _ =
    genericDeclareNamedSchema defaultSchemaOptions { Data.Swagger.sumEncoding = defaultTaggedObject } (Proxy :: Proxy Light)

instance Arbitrary LightTaggedObject where
  arbitrary = LightTaggedObject <$> arbitrary

newtype LightUntaggedValue = LightUntaggedValue Light
  deriving (Show)

instance ToJSON LightUntaggedValue where
  toJSON (LightUntaggedValue light) = genericToJSON defaultOptions { Data.Aeson.Types.sumEncoding = UntaggedValue } light

instance ToSchema LightUntaggedValue where
  declareNamedSchema _ =
    genericDeclareNamedSchema defaultSchemaOptions { Data.Swagger.sumEncoding = UntaggedValue } (Proxy :: Proxy Light)

instance Arbitrary LightUntaggedValue where
  arbitrary = LightUntaggedValue <$> arbitrary

-- ========================================================================
-- ButtonImages (bounded enum key mapping)
-- ========================================================================

data ButtonState = Neutral | Focus | Active | Hover | Disabled
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance ToJSON ButtonState
instance ToSchema ButtonState
instance ToJSONKey ButtonState where toJSONKey = toJSONKeyText (T.pack . show)

instance Arbitrary ButtonState where
  arbitrary = arbitraryBoundedEnum

type ImageUrl = T.Text

newtype ButtonImages = ButtonImages { getButtonImages :: Map ButtonState ImageUrl }
  deriving (Show, Generic)

instance ToJSON ButtonImages where
  toJSON = toJSON . getButtonImages

instance ToSchema ButtonImages where
  declareNamedSchema = genericDeclareNamedSchemaNewtype defaultSchemaOptions
    declareSchemaBoundedEnumKeyMapping

invalidButtonImagesToJSON :: ButtonImages -> Value
invalidButtonImagesToJSON = genericToJSON defaultOptions

instance Arbitrary ButtonImages where
  arbitrary = ButtonImages <$> arbitrary

-- ========================================================================
-- FreeForm (wraps a raw JSON Value)
-- ========================================================================

data FreeForm = FreeForm { jsonContent :: Map T.Text Value }
  deriving (Show, Generic)

instance ToJSON FreeForm where
  toJSON = toJSON . jsonContent

instance ToSchema FreeForm where
  declareNamedSchema _ = pure $ NamedSchema (Just $ T.pack "FreeForm") $ mempty
    & type_ ?~ SwaggerObject
    & additionalProperties ?~ AdditionalPropertiesAllowed True

instance Arbitrary FreeForm where
  arbitrary = (FreeForm . fromList) <$> genObj
    where
      genObj = listOf $ do
        k <- arbitrary
        v <- oneof [ String <$> arbitrary, Number <$> arbitrary, Bool <$> arbitrary, pure Null ]
        pure (k, v)

instance Eq ZonedTime where
  ZonedTime t (TimeZone x _ _) == ZonedTime t' (TimeZone y _ _) = t == t' && x == y

-- ========================================================================
-- Arbitrary instance for Data.Aeson.Value
-- ========================================================================

instance Arbitrary Value where
  -- Weights are almost random
  -- Uniform oneof tends not to build complex objects cause of recursive call.
  arbitrary = resize 4 $ frequency
    [ (3, Object <$> arbitrary)
    , (3, Array  <$> arbitrary)
    , (3, String <$> arbitrary)
    , (3, Number <$> arbitrary)
    , (3, Bool   <$> arbitrary)
    , (1, return Null) ]
