{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Swagger.Schema.Internal where

import Control.Lens
import Data.Aeson
import Data.Char
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Int
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Monoid
import Data.Proxy
import Data.Scientific (Scientific)
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word
import GHC.Generics

import Data.Swagger.Internal
import Data.Swagger.Lens

-- | A @'Schema'@ with an optional name.
-- This name can be used in references.
type NamedSchema = (Maybe String, Schema)

unnamed :: Schema -> NamedSchema
unnamed schema = (Nothing, schema)

named :: String -> Schema -> NamedSchema
named name schema = (Just name, schema)

-- | Convert a type into @'Schema'@.
--
-- An example type and instance:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}   -- allows to write 'T.Text' literals
-- {-\# LANGUAGE OverloadedLists \#-}     -- allows to write 'Map' as list
--
-- import Control.Lens
--
-- data Coord = Coord { x :: Double, y :: Double }
--
-- instance ToSchema Coord where
--   toNamedSchema = (Just \"Coord\", mempty
--      & schemaType .~ SchemaObject
--      & schemaProperties .~
--          [ ("x", toSchemaRef (Proxy :: Proxy Double))
--          , ("y", toSchemaRef (Proxy :: Proxy Double))
--          ]
--      & schemaRequired .~ [ "x", "y" ]
-- @
--
-- Instead of manually writing your @'ToSchema'@ instance you can
-- use a default generic implementation of @'toNamedSchema'@.
--
-- To do that, simply add @deriving 'Generic'@ clause to your datatype
-- and declare a @'ToSchema'@ instance for your datatype without
-- giving definition for @'toNamedSchema'@.
--
-- For instance, the previous example can be simplified into this:
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import GHC.Generics (Generic)
--
-- data Coord = Coord { x :: Double, y :: Double } deriving Generic
--
-- instance ToSchema Coord
-- @
class ToSchema a where
  -- | Convert a type into an optionally named schema.
  toNamedSchema :: proxy a -> NamedSchema
  default toNamedSchema :: (Generic a, GToSchema (Rep a)) => proxy a -> NamedSchema
  toNamedSchema = genericToNamedSchema defaultSchemaOptions

-- | Get type's schema name according to its @'ToSchema'@ instance.
schemaName :: ToSchema a => proxy a -> Maybe String
schemaName = fst . toNamedSchema

-- | Convert a type into a schema.
toSchema :: ToSchema a => proxy a -> Schema
toSchema = snd . toNamedSchema

-- | Convert a type into a referenced schema if possible.
-- Only named schemas can be references, nameless schemas are inlined.
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

instance ToSchema Char where
  toNamedSchema _ = unnamed $ mempty
    & schemaType .~ SchemaString
    & schemaMaxLength ?~ 1
    & schemaMinLength ?~ 1

instance ToSchema Scientific where
  toNamedSchema _ = unnamed $ mempty & schemaType .~ SchemaNumber

instance ToSchema Double where
  toNamedSchema _ = unnamed $ mempty & schemaType .~ SchemaNumber

instance ToSchema Float where
  toNamedSchema _ = unnamed $ mempty & schemaType .~ SchemaNumber

instance ToSchema a => ToSchema (Maybe a) where
  toNamedSchema _ = unnamed $ toSchema (Proxy :: Proxy a)

instance (ToSchema a, ToSchema b) => ToSchema (Either a b)

instance ToSchema ()
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

instance ToSchema IntSet where toNamedSchema _ = toNamedSchema (Proxy :: Proxy (Set Int))

-- | NOTE: This schema does not account for the uniqueness of keys.
instance ToSchema a => ToSchema (IntMap a) where
  toNamedSchema _ = toNamedSchema (Proxy :: Proxy [(Int, a)])

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

-- | Options that specify how to encode your type to Swagger schema.
data SchemaOptions = SchemaOptions
  { -- | Function applied to field labels. Handy for removing common record prefixes for example.
    fieldLabelModifier :: String -> String
    -- | Function applied to constructor tags which could be handy for lower-casing them for example.
  , constructorTagModifier :: String -> String
    -- | Function applied to datatype name.
  , datatypeNameModifier :: String -> String
    -- | If @'True'@ the constructors of a datatype, with all nullary constructors,
    -- will be encoded to a string enumeration schema with the constructor tags as possible values.
  , allNullaryToStringTag :: Bool
    -- | If @'True'@ direct subschemas will be referenced if possible (rather than inlined).
    -- Note that this option does not influence nested schemas, e.g. for these types
    --
    -- @
    -- data Object = Object String deriving Generic
    -- instance ToSchema Object
    --
    -- newtype Objects = Objects [Object] deriving Generic
    -- instance ToSchema Objects where
    --    toNamedSchema = genericToNamedSchema defaultSchemaOptions
    --      { useReferences = False }
    -- @
    --
    -- Schema for @Objects@ __will not__ inline @Object@ schema because
    -- it is nested in a @[]@ schema.
  , useReferences :: Bool
    -- | Hide the field name when a record constructor has only one field, like a newtype.
  , unwrapUnaryRecords :: Bool
  }

-- | Default encoding @'SchemaOptions'@.
--
-- @
-- 'SchemaOptions'
-- { 'fieldLabelModifier'     = id
-- , 'constructorTagModifier' = id
-- , 'datatypeNameModifier'   = id
-- , 'allNullaryToStringTag'  = True
-- , 'useReferences'          = True
-- , 'unwrapUnaryRecords'     = False
-- }
-- @
defaultSchemaOptions :: SchemaOptions
defaultSchemaOptions = SchemaOptions
  { fieldLabelModifier = id
  , constructorTagModifier = id
  , datatypeNameModifier = id
  , allNullaryToStringTag = True
  , useReferences = True
  , unwrapUnaryRecords = False
  }

-- | Default schema for @'Bounded'@, @'Integral'@ types.
toSchemaBoundedIntegral :: forall a proxy. (Bounded a, Integral a) => proxy a -> Schema
toSchemaBoundedIntegral _ = mempty
  & schemaType .~ SchemaInteger
  & schemaMinimum ?~ fromInteger (toInteger (minBound :: a))
  & schemaMaximum ?~ fromInteger (toInteger (maxBound :: a))

-- | Default generic named schema for @'Bounded'@, @'Integral'@ types.
genericToNamedSchemaBoundedIntegral :: forall a d f proxy.
  ( Bounded a, Integral a
  , Generic a, Rep a ~ D1 d f, Datatype d)
  => SchemaOptions -> proxy a -> NamedSchema
genericToNamedSchemaBoundedIntegral opts proxy
  = (gdatatypeSchemaName opts (Proxy :: Proxy d), toSchemaBoundedIntegral proxy)

-- | A configurable generic @'Schema'@ creator.
genericToSchema :: (Generic a, GToSchema (Rep a)) => SchemaOptions -> proxy a -> Schema
genericToSchema opts = snd . genericToNamedSchema opts

-- | A configurable generic @'NamedSchema'@ creator.
-- This function applied to @'defaultSchemaOptions'@
-- is used as the default for @'toNamedSchema'@
-- when the type is an instance of @'Generic'@.
genericToNamedSchema :: forall a proxy. (Generic a, GToSchema (Rep a)) => SchemaOptions -> proxy a -> NamedSchema
genericToNamedSchema opts _ = gtoNamedSchema opts (Proxy :: Proxy (Rep a)) mempty

gdatatypeSchemaName :: forall proxy d. Datatype d => SchemaOptions -> proxy d -> Maybe String
gdatatypeSchemaName opts _ = case name of
  (c:_) | isAlpha c && isUpper c -> Just name
  _ -> Nothing
  where
    name = datatypeNameModifier opts (datatypeName (Proxy3 :: Proxy3 d f a))

nullarySchema :: Schema
nullarySchema = mempty
  & schemaType .~ SchemaArray
  & schemaEnum ?~ [ toJSON () ]

instance GToSchema U1 where
  gtoNamedSchema _ _ _ = unnamed nullarySchema

instance (GToSchema f, GToSchema g) => GToSchema (f :*: g) where
  gtoNamedSchema opts _ = unnamed . gtoSchema opts (Proxy :: Proxy f) . gtoSchema opts (Proxy :: Proxy g)

instance (Datatype d, GToSchema f) => GToSchema (D1 d f) where
  gtoNamedSchema opts _ s = (schemaName, gtoSchema opts (Proxy :: Proxy f) s)
    where
      schemaName = gdatatypeSchemaName opts (Proxy :: Proxy d)

instance {-# OVERLAPPABLE #-} GToSchema f => GToSchema (C1 c f) where
  gtoNamedSchema opts _ = unnamed . gtoSchema opts (Proxy :: Proxy f)

-- | Single field constructor.
instance (Selector s, GToSchema f) => GToSchema (C1 c (S1 s f)) where
  gtoNamedSchema opts _ s
    | unwrapUnaryRecords opts = fieldSchema
    | otherwise =
        case schema ^. schemaItems of
          Just (SchemaItemsArray [_]) -> fieldSchema
          _ -> unnamed schema
    where
      schema      = gtoSchema opts (Proxy :: Proxy (S1 s f)) s
      fieldSchema = gtoNamedSchema opts (Proxy :: Proxy f) s

gtoSchemaRef :: GToSchema f => SchemaOptions -> proxy f -> Referenced Schema
gtoSchemaRef opts proxy = case gtoNamedSchema opts proxy mempty of
  (Just name, _)
    | useReferences opts -> Ref (Reference ("#/definitions/" <> T.pack name))
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

instance (GSumToSchema f, GSumToSchema g) => GToSchema (f :+: g) where
  gtoNamedSchema opts _ s
    | allNullaryToStringTag opts && allNullary = unnamed (toStringTag sumSchema)
    | otherwise = unnamed sumSchema
    where
      (All allNullary, sumSchema) = gsumToSchema opts (Proxy :: Proxy (f :+: g)) s

      toStringTag schema = mempty
        & schemaType .~ SchemaString
        & schemaEnum ?~ map toJSON (schema ^.. schemaProperties.ifolded.asIndex)

type AllNullary = All

class GSumToSchema f where
  gsumToSchema :: SchemaOptions -> proxy f -> Schema -> (AllNullary, Schema)

instance (GSumToSchema f, GSumToSchema g) => GSumToSchema (f :+: g) where
  gsumToSchema opts _ = gsumToSchema opts (Proxy :: Proxy f) <.> gsumToSchema opts (Proxy :: Proxy g)
    where
      (f <.> g) s = (a <> b, s'')
        where
          (a, s')  = f s
          (b, s'') = g s'

gsumConToSchema :: forall c f proxy. Constructor c =>
  Bool -> Referenced Schema -> SchemaOptions -> proxy (C1 c f) -> Schema -> (AllNullary, Schema)
gsumConToSchema isNullary tagSchemaRef opts _ schema = (All isNullary, schema
  & schemaType .~ SchemaObject
  & schemaProperties . at tag ?~ tagSchemaRef
  & schemaMaxProperties ?~ 1
  & schemaMinProperties ?~ 1)
  where
    tag = T.pack (constructorTagModifier opts (conName (Proxy3 :: Proxy3 c f p)))

instance {-# OVERLAPPABLE #-} (Constructor c, GToSchema f) => GSumToSchema (C1 c f) where
  gsumToSchema opts = gsumConToSchema False tagSchemaRef opts
    where
      tagSchemaRef = gtoSchemaRef opts (Proxy :: Proxy (C1 c f))

instance Constructor c => GSumToSchema (C1 c U1) where
  gsumToSchema opts = gsumConToSchema True tagSchemaRef opts
    where
      tagSchemaRef = gtoSchemaRef opts (Proxy :: Proxy (C1 c U1))

instance (Constructor c, Selector s, GToSchema f) => GSumToSchema (C1 c (S1 s f)) where
  gsumToSchema opts = gsumConToSchema False tagSchemaRef opts
    where
      tagSchemaRef = gtoSchemaRef opts (Proxy :: Proxy (C1 c (S1 s f)))

data Proxy2 a b = Proxy2

data Proxy3 a b c = Proxy3

