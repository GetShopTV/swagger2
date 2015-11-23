module SpecCommon where

import Data.Aeson
import Data.Aeson.QQ
import qualified Data.Foldable as F
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Monoid
import qualified Data.Vector as Vector
import Data.Text (Text)

import Data.Swagger

import Test.Hspec

isSubJSON :: Value -> Value -> Bool
isSubJSON Null _ = True
isSubJSON (Object x) (Object y) = HashMap.keys x == HashMap.keys i && F.and i
  where
    i = HashMap.intersectionWith isSubJSON x y
isSubJSON (Array xs) (Array ys) = Vector.length xs == Vector.length ys && F.and (Vector.zipWith isSubJSON xs ys)
isSubJSON x y = x == y

(<~>) :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Value -> Spec
x <~> json = do
  it "encodes correctly (probably with extra properties)" $ do
    toJSON x `shouldSatisfy` (json `isSubJSON`)
  it "decodes correctly" $ do
    fromJSON json `shouldBe` Success x

(<=>) :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Value -> Spec
x <=> json = do
  it "encodes correctly" $ do
    toJSON x `shouldBe` json
  it "decodes correctly" $ do
    fromJSON json `shouldBe` Success x


