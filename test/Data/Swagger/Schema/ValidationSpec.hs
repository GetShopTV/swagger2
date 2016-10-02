{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Swagger.Schema.ValidationSpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Int
import Data.IntMap (IntMap)
import Data.Hashable (Hashable)
import "unordered-containers" Data.HashSet (HashSet)
import qualified "unordered-containers" Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import Data.Proxy
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Set (Set)
import Data.Word
import GHC.Generics

import Data.Swagger

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

shouldValidate :: (ToJSON a, ToSchema a) => Proxy a -> a -> Bool
shouldValidate _ x = validateToJSON x == []

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
    prop "(HashSet Bool)" $ shouldValidate (Proxy :: Proxy (HashSet Bool))
    prop "(Either Int String)" $ shouldValidate (Proxy :: Proxy (Either Int String))
    prop "(Int, String)" $ shouldValidate (Proxy :: Proxy (Int, String))
    prop "(Map String Int)" $ shouldValidate (Proxy :: Proxy (Map String Int))
    prop "(Map T.Text Int)" $ shouldValidate (Proxy :: Proxy (Map T.Text Int))
    prop "(Map TL.Text Bool)" $ shouldValidate (Proxy :: Proxy (Map TL.Text Bool))
    prop "(HashMap String Int)" $ shouldValidate (Proxy :: Proxy (HashMap String Int))
    prop "(HashMap T.Text Int)" $ shouldValidate (Proxy :: Proxy (HashMap T.Text Int))
    prop "(HashMap TL.Text Bool)" $ shouldValidate (Proxy :: Proxy (HashMap TL.Text Bool))
    prop "(Int, String, Double)" $ shouldValidate (Proxy :: Proxy (Int, String, Double))
    prop "(Int, String, Double, [Int])" $ shouldValidate (Proxy :: Proxy (Int, String, Double, [Int]))
    prop "(Int, String, Double, [Int], Int)" $ shouldValidate (Proxy :: Proxy (Int, String, Double, [Int], Int))
    prop "Person" $ shouldValidate (Proxy :: Proxy Person)
    prop "Color" $ shouldValidate (Proxy :: Proxy Color)
    prop "Paint" $ shouldValidate (Proxy :: Proxy Paint)
    prop "MyRoseTree" $ shouldValidate (Proxy :: Proxy MyRoseTree)
    prop "Light" $ shouldValidate (Proxy :: Proxy Light)

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

-- ========================================================================
-- Color (enum)
-- ========================================================================
data Color = Red | Green | Blue deriving (Show, Generic, Bounded, Enum)

instance ToJSON Color
instance ToSchema Color

instance Arbitrary Color where
  arbitrary = arbitraryBoundedEnum

-- ========================================================================
-- Paint (record with bounded enum property)
-- ========================================================================

newtype Paint = Paint { color :: Color }
  deriving (Show, Generic)

instance ToJSON Paint
instance ToSchema Paint

instance Arbitrary Paint where
  arbitrary = Paint <$> arbitrary

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

instance ToSchema Light

instance ToJSON Light where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }

instance Arbitrary Light where
  arbitrary = oneof
    [ return NoLight
    , LightFreq <$> arbitrary
    , LightColor <$> arbitrary
    ]

-- Arbitrary instances for common types

instance (Eq k, Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (HashMap k v) where
  arbitrary = HashMap.fromList <$> arbitrary

instance (Eq a, Hashable a, Arbitrary a) => Arbitrary (HashSet a) where
  arbitrary = HashSet.fromList <$> arbitrary

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TL.Text where
  arbitrary = TL.pack <$> arbitrary

instance Arbitrary Day where
  arbitrary = liftA3 fromGregorian (fmap ((+ 1) . abs) arbitrary) arbitrary arbitrary

instance Arbitrary LocalTime where
  arbitrary = LocalTime
    <$> arbitrary
    <*> liftA3 TimeOfDay (choose (0, 23)) (choose (0, 59)) (fromInteger <$> choose (0, 60))

instance Eq ZonedTime where
  ZonedTime t (TimeZone x _ _) == ZonedTime t' (TimeZone y _ _) = t == t' && x == y

instance Arbitrary ZonedTime where
  arbitrary = ZonedTime
    <$> arbitrary
    <*> liftA3 TimeZone arbitrary arbitrary (vectorOf 3 (elements ['A'..'Z']))

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> fmap fromInteger (choose (0, 86400))

