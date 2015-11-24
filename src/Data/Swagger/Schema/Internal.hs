{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Swagger.Schema.Internal where

import Control.Lens
import Data.Aeson
import Data.Char
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Int
import Data.Map (Map)
import Data.Monoid
import Data.Proxy
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word
import GHC.Generics

import Data.Swagger.Internal
import Data.Swagger.Lens

type NamedSchema = (Maybe String, Schema)

unnamed :: Schema -> NamedSchema
unnamed schema = (Nothing, schema)

named :: String -> Schema -> NamedSchema
named name schema = (Just name, schema)

class ToSchema a where
  toNamedSchema :: proxy a -> NamedSchema
  default toNamedSchema :: (Generic a, GToSchema (Rep a)) => proxy a -> NamedSchema
  toNamedSchema = genericToNamedSchema defaultSchemaOptions

schemaName :: ToSchema a => proxy a -> Maybe String
schemaName = fst . toNamedSchema

toSchema :: ToSchema a => proxy a -> Schema
toSchema = snd . toNamedSchema

toSchemaRef :: ToSchema a => proxy a -> Referenced Schema
toSchemaRef proxy = case toNamedSchema proxy of
  (Just name, _)  -> Ref (Reference ("#/definitions/" <> T.pack name))
  (_, schema)     -> Inline schema

class GToSchema (f :: * -> *) where
  gtoNamedSchema :: SchemaOptions -> proxy f -> Schema -> NamedSchema

gtoSchema :: GToSchema f => SchemaOptions -> proxy f -> Schema -> Schema
gtoSchema opts proxy = snd . gtoNamedSchema opts proxy

instance {-# OVERLAPPABLE #-} ToSchema a => ToSchema [a] where
  toNamedSchema _ = unnamed $ mempty
    & schemaType  .~ SchemaArray
    & schemaItems ?~ SchemaItemsObject (toSchemaRef (Proxy :: Proxy a))

instance {-# OVERLAPPING #-} ToSchema String where
  toNamedSchema _ = unnamed $ mempty & schemaType .~ SchemaString

instance ToSchema Bool where
  toNamedSchema _ = unnamed $ mempty & schemaType .~ SchemaBoolean

instance ToSchema Integer where
  toNamedSchema _ = unnamed $ mempty & schemaType .~ SchemaInteger

instance ToSchema Int    where toNamedSchema = unnamed . toSchemaBoundedIntegral
instance ToSchema Int8   where toNamedSchema = unnamed . toSchemaBoundedIntegral
instance ToSchema Int16  where toNamedSchema = unnamed . toSchemaBoundedIntegral
instance ToSchema Int32  where toNamedSchema = unnamed . toSchemaBoundedIntegral
instance ToSchema Int64  where toNamedSchema = unnamed . toSchemaBoundedIntegral

instance ToSchema Word   where toNamedSchema = unnamed . toSchemaBoundedIntegral
instance ToSchema Word8  where toNamedSchema = unnamed . toSchemaBoundedIntegral
instance ToSchema Word16 where toNamedSchema = unnamed . toSchemaBoundedIntegral
instance ToSchema Word32 where toNamedSchema = unnamed . toSchemaBoundedIntegral
instance ToSchema Word64 where toNamedSchema = unnamed . toSchemaBoundedIntegral

instance ToSchema Double where
  toNamedSchema _ = unnamed $ mempty & schemaType .~ SchemaNumber

instance ToSchema Float where
  toNamedSchema _ = unnamed $ mempty & schemaType .~ SchemaNumber

instance ToSchema a => ToSchema (Maybe a) where
  toNamedSchema _ = unnamed $ toSchema (Proxy :: Proxy a)

instance (ToSchema a, ToSchema b) => ToSchema (a, b)
instance (ToSchema a, ToSchema b, ToSchema c) => ToSchema (a, b, c)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d) => ToSchema (a, b, c, d)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d, ToSchema e) => ToSchema (a, b, c, d, e)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d, ToSchema e, ToSchema f) => ToSchema (a, b, c, d, e, f)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d, ToSchema e, ToSchema f, ToSchema g) => ToSchema (a, b, c, d, e, f, g)

instance ToSchema T.Text where
  toNamedSchema _ = unnamed $ toSchema (Proxy :: Proxy String)

instance ToSchema TL.Text where
  toNamedSchema _ = unnamed $ toSchema (Proxy :: Proxy String)

instance ToSchema a => ToSchema (Map String a) where
  toNamedSchema _ = unnamed $ mempty
    & schemaType  .~ SchemaObject
    & schemaAdditionalProperties ?~ toSchema (Proxy :: Proxy a)

instance ToSchema a => ToSchema (Map T.Text  a) where toNamedSchema _ = unnamed $ toSchema (Proxy :: Proxy (Map String a))
instance ToSchema a => ToSchema (Map TL.Text a) where toNamedSchema _ = unnamed $ toSchema (Proxy :: Proxy (Map String a))

instance ToSchema a => ToSchema (HashMap String  a) where toNamedSchema _ = unnamed $ toSchema (Proxy :: Proxy (Map String a))
instance ToSchema a => ToSchema (HashMap T.Text  a) where toNamedSchema _ = unnamed $ toSchema (Proxy :: Proxy (Map String a))
instance ToSchema a => ToSchema (HashMap TL.Text a) where toNamedSchema _ = unnamed $ toSchema (Proxy :: Proxy (Map String a))

instance ToSchema a => ToSchema (Set a) where
  toNamedSchema _ = unnamed $ toSchema (Proxy :: Proxy [a])
    & schemaUniqueItems ?~ True

instance ToSchema a => ToSchema (HashSet a) where toNamedSchema _ = unnamed $ toSchema (Proxy :: Proxy (Set a))

instance ToSchema All where toNamedSchema _ = unnamed $ toSchema (Proxy :: Proxy Bool)
instance ToSchema Any where toNamedSchema _ = unnamed $ toSchema (Proxy :: Proxy Bool)
instance ToSchema a => ToSchema (Sum a)     where toNamedSchema _ = unnamed $ toSchema (Proxy :: Proxy a)
instance ToSchema a => ToSchema (Product a) where toNamedSchema _ = unnamed $ toSchema (Proxy :: Proxy a)
instance ToSchema a => ToSchema (First a)   where toNamedSchema _ = unnamed $ toSchema (Proxy :: Proxy a)
instance ToSchema a => ToSchema (Last a)    where toNamedSchema _ = unnamed $ toSchema (Proxy :: Proxy a)
instance ToSchema a => ToSchema (Dual a)    where toNamedSchema _ = unnamed $ toSchema (Proxy :: Proxy a)

data SchemaOptions = SchemaOptions
  { fieldLabelModifier :: String -> String
  , datatypeNameModifier :: String -> String
  , unwrapUnaryRecords :: Bool
  }

defaultSchemaOptions :: SchemaOptions
defaultSchemaOptions = SchemaOptions
  { fieldLabelModifier = id
  , datatypeNameModifier = id
  , unwrapUnaryRecords = False
  }

toSchemaBoundedIntegral :: forall a proxy. (Bounded a, Integral a) => proxy a -> Schema
toSchemaBoundedIntegral _ = mempty
  & schemaType .~ SchemaInteger
  & schemaMinimum ?~ toInteger (minBound :: a)
  & schemaMaximum ?~ toInteger (maxBound :: a)

toSchemaBoundedEnum :: forall a proxy. (ToJSON a, Bounded a, Enum a) => proxy a -> Schema
toSchemaBoundedEnum _ = mempty
  & schemaType .~ SchemaString
  & schemaEnum ?~ map toJSON [minBound..maxBound :: a]

genericToNamedSchemaBoundedEnum :: (ToJSON a, Bounded a, Enum a, Generic a) => proxy a -> NamedSchema
genericToNamedSchemaBoundedEnum = unnamed . toSchemaBoundedEnum

genericToSchema :: (Generic a, GToSchema (Rep a)) => SchemaOptions -> proxy a -> Schema
genericToSchema opts = snd . genericToNamedSchema opts

genericToNamedSchema :: forall a proxy. (Generic a, GToSchema (Rep a)) => SchemaOptions -> proxy a -> NamedSchema
genericToNamedSchema opts _ = gtoNamedSchema opts (Proxy :: Proxy (Rep a)) mempty

gdatatypeSchemaName :: forall proxy d. Datatype d => SchemaOptions -> proxy d -> Maybe String
gdatatypeSchemaName opts _ = case name of
  (c:_) | isAlpha c && isUpper c -> Just name
  _ -> Nothing
  where
    name = datatypeNameModifier opts (datatypeName (Proxy3 :: Proxy3 d f a))

instance (GToSchema f, GToSchema g) => GToSchema (f :*: g) where
  gtoNamedSchema opts _ = unnamed . gtoSchema opts (Proxy :: Proxy f) . gtoSchema opts (Proxy :: Proxy g)

-- | Single constructor data type.
instance {-# OVERLAPPABLE #-} (Datatype d, GToSchema f) => GToSchema (D1 d (C1 c f)) where
  gtoNamedSchema opts _ s = (schemaName, gtoSchema opts (Proxy :: Proxy f) s)
    where
      schemaName = gdatatypeSchemaName opts (Proxy :: Proxy d)

-- | Single field single constructor data type.
instance (Datatype d, Selector s, GToSchema f) => GToSchema (D1 d (C1 c (S1 s f))) where
  gtoNamedSchema opts _ s
    | unwrapUnaryRecords opts = (schemaName, gtoSchema opts (Proxy :: Proxy f) s)
    | otherwise = (schemaName,) $
        case schema ^. schemaItems of
          Just (SchemaItemsArray [_]) -> fieldSchema
          _ -> schema
    where
      schemaName  = gdatatypeSchemaName opts (Proxy :: Proxy d)
      schema      = gtoSchema opts (Proxy :: Proxy (S1 s f)) s
      fieldSchema = gtoSchema opts (Proxy :: Proxy f) s

gtoSchemaRef :: GToSchema f => SchemaOptions -> proxy f -> Referenced Schema
gtoSchemaRef opts proxy = case gtoNamedSchema opts proxy mempty of
  (Just name, _)  -> Ref (Reference ("#/definitions/" <> T.pack name))
  (_, schema)     -> Inline schema

appendItem :: Referenced Schema -> Maybe SchemaItems -> Maybe SchemaItems
appendItem x Nothing = Just (SchemaItemsArray [x])
appendItem x (Just (SchemaItemsArray xs)) = Just (SchemaItemsArray (x:xs))
appendItem _ _ = error "GToSchema.appendItem: cannot append to SchemaItemsObject"

withFieldSchema :: forall proxy s f. (Selector s, GToSchema f) => SchemaOptions -> proxy s f -> Bool -> Schema -> Schema
withFieldSchema opts _ isRequiredField schema
  | T.null fieldName = schema
      & schemaType .~ SchemaArray
      & schemaItems %~ appendItem fieldSchemaRef
  | otherwise = schema
      & schemaType .~ SchemaObject
      & schemaProperties . at fieldName ?~ fieldSchemaRef
      & if isRequiredField
          then schemaRequired %~ (fieldName :)
          else id
  where
    fieldName = T.pack (fieldLabelModifier opts (selName (Proxy3 :: Proxy3 s f p)))
    fieldSchemaRef = gtoSchemaRef opts (Proxy :: Proxy f)

-- | Optional record fields.
instance {-# OVERLAPPING #-} (Selector s, ToSchema c) => GToSchema (S1 s (K1 i (Maybe c))) where
  gtoNamedSchema opts _ = unnamed . withFieldSchema opts (Proxy2 :: Proxy2 s (K1 i (Maybe c))) False

-- | Record fields.
instance {-# OVERLAPPABLE #-} (Selector s, GToSchema f) => GToSchema (S1 s f) where
  gtoNamedSchema opts _ = unnamed . withFieldSchema opts (Proxy2 :: Proxy2 s f) True

instance ToSchema c => GToSchema (K1 i c) where
  gtoNamedSchema _ _ _ = toNamedSchema (Proxy :: Proxy c)

data Proxy2 a b = Proxy2

data Proxy3 a b c = Proxy3

