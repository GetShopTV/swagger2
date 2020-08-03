{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Data.Swagger.Internal.Utils where

import Prelude ()
import Prelude.Compat

import Control.Lens ((&), (%~))
import Control.Lens.TH
import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Data.Data
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics
import Language.Haskell.TH (mkName)

swaggerFieldRules :: LensRules
swaggerFieldRules = defaultFieldRules & lensField %~ swaggerFieldNamer
  where
    swaggerFieldNamer namer dname fnames fname =
      map fixDefName (namer dname fnames fname)

    fixDefName (MethodName cname mname) = MethodName cname (fixName mname)
    fixDefName (TopName name) = TopName (fixName name)

    fixName = mkName . fixName' . show

    fixName' "in"       = "in_"       -- keyword
    fixName' "type"     = "type_"     -- keyword
    fixName' "default"  = "default_"  -- keyword
    fixName' "minimum"  = "minimum_"  -- Prelude conflict
    fixName' "maximum"  = "maximum_"  -- Prelude conflict
    fixName' "enum"     = "enum_"     -- Control.Lens conflict
    fixName' "head"     = "head_"     -- Prelude conflict
    fixName' "not"      = "not_"      -- Prelude conflict
    fixName' n = n

gunfoldEnum :: String -> [a] -> (forall b r. Data b => c (b -> r) -> c r) -> (forall r. r -> c r) -> Constr -> c a
gunfoldEnum tname xs _k z c = case lookup (constrIndex c) (zip [1..] xs) of
  Just x -> z x
  Nothing -> error $ "Data.Data.gunfold: Constructor " ++ show c ++ " is not of type " ++ tname ++ "."

jsonPrefix :: String -> Options
jsonPrefix prefix = defaultOptions
  { fieldLabelModifier      = modifier . drop 1
  , constructorTagModifier  = modifier
  , sumEncoding             = ObjectWithSingleField
  , omitNothingFields       = True
  }
  where
    modifier = lowerFirstUppers . drop (length prefix)

    lowerFirstUppers s = map toLower x ++ y
      where (x, y) = span isUpper s

parseOneOf :: ToJSON a => [a] -> Value -> Parser a
parseOneOf xs js =
  case lookup js ys of
    Nothing -> fail $ "invalid json: " ++ show js ++ " (expected one of " ++ show (map fst ys) ++ ")"
    Just x  -> pure x
  where
    ys = zip (map toJSON xs) xs

(<+>) :: Value -> Value -> Value
Object x <+> Object y = Object (x <> y)
_ <+> _ = error "<+>: merging non-objects"

genericMempty :: (Generic a, GMonoid (Rep a)) => a
genericMempty = to gmempty

genericMappend :: (Generic a, GMonoid (Rep a)) => a -> a -> a
genericMappend x y = to (gmappend (from x) (from y))

class GMonoid f where
  gmempty :: f p
  gmappend :: f p -> f p -> f p

instance GMonoid U1 where
  gmempty = U1
  gmappend _ _ = U1

instance (GMonoid f, GMonoid g) => GMonoid (f :*: g) where
  gmempty = gmempty :*: gmempty
  gmappend (a :*: x) (b :*: y) = gmappend a b :*: gmappend x y

instance SwaggerMonoid a => GMonoid (K1 i a) where
  gmempty = K1 swaggerMempty
  gmappend (K1 x) (K1 y) = K1 (swaggerMappend x y)

instance GMonoid f => GMonoid (M1 i t f) where
  gmempty = M1 gmempty
  gmappend (M1 x) (M1 y) = M1 (gmappend x y)

class SwaggerMonoid m where
  swaggerMempty :: m
  swaggerMappend :: m -> m -> m
  default swaggerMempty :: Monoid m => m
  swaggerMempty = mempty
  default swaggerMappend :: Monoid m => m -> m -> m
  swaggerMappend = mappend

instance SwaggerMonoid [a]
instance Ord a => SwaggerMonoid (Set a)
instance Ord k => SwaggerMonoid (Map k v)

instance (Eq k, Hashable k) => SwaggerMonoid (HashMap k v) where
  swaggerMempty = mempty
  swaggerMappend = HashMap.unionWith (\_old new -> new)

instance (Eq k, Hashable k) => SwaggerMonoid (InsOrdHashMap k v) where
  swaggerMempty = mempty
  swaggerMappend = InsOrdHashMap.unionWith (\_old new -> new)

instance SwaggerMonoid Text where
  swaggerMempty = mempty
  swaggerMappend x "" = x
  swaggerMappend _ y = y

instance SwaggerMonoid (Maybe a) where
  swaggerMempty = Nothing
  swaggerMappend x Nothing = x
  swaggerMappend _ y = y
