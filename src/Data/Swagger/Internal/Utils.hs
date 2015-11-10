{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Data.Swagger.Internal.Utils where

import Control.Arrow (first)
import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (Parser, Pair)
import Data.Char
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import Data.Text (Text)
import Data.Traversable
import GHC.Generics
import Language.Haskell.TH
import Text.Read (readMaybe)

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
  }
  where
    modifier = lowerFirstUppers . drop (length prefix)

    lowerFirstUppers s = map toLower x ++ y
      where (x, y) = span isUpper s

deriveToJSON' :: Name -> Q [Dec]
deriveToJSON' name = deriveToJSON (jsonPrefix (nameBase name)) name

deriveJSONDefault :: Name -> Q [Dec]
deriveJSONDefault = deriveJSON (jsonPrefix "")

deriveJSON' :: Name -> Q [Dec]
deriveJSON' name = deriveJSON (jsonPrefix (nameBase name)) name

genericToJSONWithSub :: (Generic a, GToJSON (Rep a)) => Text -> Options -> a -> Value
genericToJSONWithSub sub opts x =
  case genericToJSON opts x of
    Object o ->
      let so = HashMap.lookupDefault (error "impossible") sub o
      in Object (HashMap.delete sub o) <+> so
    _ -> error "impossible"

genericParseJSONWithSub :: (Generic a, GFromJSON (Rep a)) => Text -> Options -> Value -> Parser a
genericParseJSONWithSub sub opts (Object o) = genericParseJSON opts json
  where
    json = Object (HashMap.insert sub (Object o) o)
genericParseJSONWithSub _ _ _ = error "impossible"

(<+>) :: Value -> Value -> Value
Object x <+> Object y = Object (x <> y)
_ <+> _ = error "impossible"

withDefaults :: (Value -> Parser a) -> [Pair] -> Value -> Parser a
withDefaults parser defs json@(Object _) = parser (json <+> object defs)
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

instance SwaggerMonoid Text where
  swaggerMempty = mempty
  swaggerMappend x "" = x
  swaggerMappend _ y = y

instance SwaggerMonoid (Maybe a) where
  swaggerMempty = Nothing
  swaggerMappend x Nothing = x
  swaggerMappend _ y = y

-- | __WARNING:__ not a true monoid (`swaggerMempty` breaks right identity law).
instance SwaggerMonoid Bool where
  swaggerMempty = False
  swaggerMappend _ y = y

