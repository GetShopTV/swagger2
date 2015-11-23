{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Swagger.Schema.Internal where

import Control.Lens
import Data.Aeson
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

class ToSchema a where
  toSchema :: proxy a -> Schema
  default toSchema :: (Generic a, GToSchema (Rep a)) => proxy a -> Schema
  toSchema = genericToSchema defaultSchemaOptions

class GToSchema (f :: * -> *) where
  gtoSchema :: SchemaOptions -> proxy f -> Schema -> Schema

instance {-# OVERLAPPABLE #-} ToSchema a => ToSchema [a] where
  toSchema _ = mempty
    & schemaType  .~ SchemaArray
    & schemaItems ?~ SchemaItemsObject (Inline (toSchema (Proxy :: Proxy a)))

instance {-# OVERLAPPING #-} ToSchema String where
  toSchema _ = mempty & schemaType .~ SchemaString

instance ToSchema Bool where
  toSchema _ = mempty & schemaType .~ SchemaBoolean

instance ToSchema Integer where
  toSchema _ = mempty & schemaType .~ SchemaInteger

instance ToSchema Int    where toSchema = toSchemaBoundedIntegral
instance ToSchema Int8   where toSchema = toSchemaBoundedIntegral
instance ToSchema Int16  where toSchema = toSchemaBoundedIntegral
instance ToSchema Int32  where toSchema = toSchemaBoundedIntegral
instance ToSchema Int64  where toSchema = toSchemaBoundedIntegral

instance ToSchema Word   where toSchema = toSchemaBoundedIntegral
instance ToSchema Word8  where toSchema = toSchemaBoundedIntegral
instance ToSchema Word16 where toSchema = toSchemaBoundedIntegral
instance ToSchema Word32 where toSchema = toSchemaBoundedIntegral
instance ToSchema Word64 where toSchema = toSchemaBoundedIntegral

instance ToSchema Double where
  toSchema _ = mempty & schemaType .~ SchemaNumber

instance ToSchema Float where
  toSchema _ = mempty & schemaType .~ SchemaNumber

instance ToSchema a => ToSchema (Maybe a) where
  toSchema _ = toSchema (Proxy :: Proxy a)

instance (ToSchema a, ToSchema b) => ToSchema (a, b)
instance (ToSchema a, ToSchema b, ToSchema c) => ToSchema (a, b, c)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d) => ToSchema (a, b, c, d)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d, ToSchema e) => ToSchema (a, b, c, d, e)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d, ToSchema e, ToSchema f) => ToSchema (a, b, c, d, e, f)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d, ToSchema e, ToSchema f, ToSchema g) => ToSchema (a, b, c, d, e, f, g)

instance ToSchema T.Text where
  toSchema _ = toSchema (Proxy :: Proxy String)

instance ToSchema TL.Text where
  toSchema _ = toSchema (Proxy :: Proxy String)

instance ToSchema a => ToSchema (Map String a) where
  toSchema _ = mempty
    & schemaType  .~ SchemaObject
    & schemaAdditionalProperties ?~ toSchema (Proxy :: Proxy a)

instance ToSchema a => ToSchema (Map T.Text  a) where toSchema _ = toSchema (Proxy :: Proxy (Map String a))
instance ToSchema a => ToSchema (Map TL.Text a) where toSchema _ = toSchema (Proxy :: Proxy (Map String a))

instance ToSchema a => ToSchema (HashMap String  a) where toSchema _ = toSchema (Proxy :: Proxy (Map String a))
instance ToSchema a => ToSchema (HashMap T.Text  a) where toSchema _ = toSchema (Proxy :: Proxy (Map String a))
instance ToSchema a => ToSchema (HashMap TL.Text a) where toSchema _ = toSchema (Proxy :: Proxy (Map String a))

instance ToSchema a => ToSchema (Set a) where
  toSchema _ = toSchema (Proxy :: Proxy [a])
    & schemaUniqueItems ?~ True

instance ToSchema a => ToSchema (HashSet a) where toSchema _ = toSchema (Proxy :: Proxy (Set a))

instance ToSchema All where toSchema _ = toSchema (Proxy :: Proxy Bool)
instance ToSchema Any where toSchema _ = toSchema (Proxy :: Proxy Bool)
instance ToSchema a => ToSchema (Sum a)     where toSchema _ = toSchema (Proxy :: Proxy a)
instance ToSchema a => ToSchema (Product a) where toSchema _ = toSchema (Proxy :: Proxy a)
instance ToSchema a => ToSchema (First a)   where toSchema _ = toSchema (Proxy :: Proxy a)
instance ToSchema a => ToSchema (Last a)    where toSchema _ = toSchema (Proxy :: Proxy a)
instance ToSchema a => ToSchema (Dual a)    where toSchema _ = toSchema (Proxy :: Proxy a)

data SchemaOptions = SchemaOptions
  { fieldLabelModifier :: String -> String
  , unwrapUnaryRecords :: Bool
  }

defaultSchemaOptions :: SchemaOptions
defaultSchemaOptions = SchemaOptions
  { fieldLabelModifier = id
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

genericToSchema :: forall a proxy. (Generic a, GToSchema (Rep a)) => SchemaOptions -> proxy a -> Schema
genericToSchema opts _ = gtoSchema opts (Proxy :: Proxy (Rep a)) mempty

instance (GToSchema f, GToSchema g) => GToSchema (f :*: g) where
  gtoSchema opts _ = gtoSchema opts (Proxy :: Proxy f) . gtoSchema opts (Proxy :: Proxy g)

-- | Single constructor data type.
instance {-# OVERLAPPABLE #-} GToSchema f => GToSchema (D1 d (C1 c f)) where
  gtoSchema opts _ = gtoSchema opts (Proxy :: Proxy f)

-- | Single field single constructor data type.
instance (Selector s, GToSchema f) => GToSchema (D1 d (C1 c (S1 s f))) where
  gtoSchema opts _ s
    | unwrapUnaryRecords opts = gtoSchema opts (Proxy :: Proxy f) s
    | otherwise = case schema ^. schemaItems of
        Just (SchemaItemsArray [Inline fieldSchema]) -> fieldSchema
        _ -> schema
    where
      schema = gtoSchema opts (Proxy :: Proxy (S1 s f)) s

appendItem :: Referenced Schema -> Maybe SchemaItems -> Maybe SchemaItems
appendItem x Nothing = Just (SchemaItemsArray [x])
appendItem x (Just (SchemaItemsArray xs)) = Just (SchemaItemsArray (x:xs))
appendItem _ _ = error "GToSchema.appendItem: cannot append to SchemaItemsObject"

withFieldSchema :: forall proxy s f. (Selector s, GToSchema f) => SchemaOptions -> proxy s f -> Bool -> Schema -> Schema
withFieldSchema opts _ isRequiredField schema
  | T.null fieldName = schema
      & schemaType .~ SchemaArray
      & schemaItems %~ appendItem (Inline fieldSchema)
  | otherwise = schema
      & schemaType .~ SchemaObject
      & schemaProperties . at fieldName ?~ Inline fieldSchema
      & if isRequiredField
          then schemaRequired %~ (fieldName :)
          else id
  where
    fieldName = T.pack (fieldLabelModifier opts (selName (Proxy3 :: Proxy3 s f p)))
    fieldSchema = gtoSchema opts (Proxy :: Proxy f) mempty

-- | Optional record fields.
instance {-# OVERLAPPING #-} (Selector s, ToSchema c) => GToSchema (S1 s (K1 i (Maybe c))) where
  gtoSchema opts _ = withFieldSchema opts (Proxy2 :: Proxy2 s (K1 i (Maybe c))) False

-- | Record fields.
instance {-# OVERLAPPABLE #-} (Selector s, GToSchema f) => GToSchema (S1 s f) where
  gtoSchema opts _ = withFieldSchema opts (Proxy2 :: Proxy2 s f) True

instance ToSchema c => GToSchema (K1 i c) where
  gtoSchema _ _ _ = toSchema (Proxy :: Proxy c)

data Proxy2 a b = Proxy2

data Proxy3 a b c = Proxy3

