{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Data.Swagger.Internal.Utils where

import Control.Arrow (first)
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Data.Data
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import Data.Monoid
import Data.Text (Text)
import GHC.Generics
import Text.Read (readMaybe)

gunfoldEnum :: String -> [a] -> (forall b r. Data b => c (b -> r) -> c r) -> (forall r. r -> c r) -> Constr -> c a
gunfoldEnum tname xs _k z c = case lookup (constrIndex c) (zip [1..] xs) of
  Just x -> z x
  Nothing -> error $ "Data.Data.gunfold: Constructor " ++ show c ++ " is not of type " ++ tname ++ "."

hashMapMapKeys :: (Eq k', Hashable k') => (k -> k') -> HashMap k v -> HashMap k' v
hashMapMapKeys f = HashMap.fromList . map (first f) . HashMap.toList

hashMapTraverseKeys :: (Eq k', Hashable k', Applicative f) => (k -> f k') -> HashMap k v -> f (HashMap k' v)
hashMapTraverseKeys f = fmap HashMap.fromList . traverse g . HashMap.toList
  where
    g (x, y) = (\a -> (a, y)) <$> f x

hashMapReadKeys :: (Eq k, Read k, Hashable k, Alternative f) => HashMap String v -> f (HashMap k v)
hashMapReadKeys = hashMapTraverseKeys (maybe empty pure . readMaybe)

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

omitEmpties :: Value -> Value
omitEmpties (Object o) = Object (HashMap.filter nonEmpty o)
  where
    nonEmpty js = (js /= Object mempty) && (js /= Array mempty) && (js /= Null)
omitEmpties js = js

genericToJSONWithSub :: (Generic a, GToJSON (Rep a)) => Text -> Options -> a -> Value
genericToJSONWithSub sub opts x =
  case genericToJSON opts x of
    Object o ->
      case HashMap.lookup sub o of
        Just so -> Object (HashMap.delete sub o) <+> so
        Nothing -> Object o -- no subjson, leaving object as is
    _ -> error "genericToJSONWithSub: subjson is not an object"

genericParseJSONWithSub :: (Generic a, GFromJSON (Rep a)) => Text -> Options -> Value -> Parser a
genericParseJSONWithSub sub opts js@(Object o)
    = genericParseJSON opts js    -- try without subjson
  <|> genericParseJSON opts js'   -- try with subjson
  where
    js' = Object (HashMap.insert sub (Object o) o)
genericParseJSONWithSub _ _ _ = error "genericParseJSONWithSub: given json is not an object"

(<+>) :: Value -> Value -> Value
Object x <+> Object y = Object (x <> y)
_ <+> _ = error "<+>: merging non-objects"

withDefaults :: (Value -> Parser a) -> [Pair] -> Value -> Parser a
withDefaults parser defs js@(Object _) = parser (js <+> object defs)
withDefaults _ _ _ = empty

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
instance Ord k => SwaggerMonoid (Map k v)

instance SwaggerMonoid Text where
  swaggerMempty = mempty
  swaggerMappend x "" = x
  swaggerMappend _ y = y

instance SwaggerMonoid (Maybe a) where
  swaggerMempty = Nothing
  swaggerMappend x Nothing = x
  swaggerMappend _ y = y

