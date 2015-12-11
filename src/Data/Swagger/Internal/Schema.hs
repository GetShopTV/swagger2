{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Swagger.Internal.Schema where

import Control.Lens
import Data.Aeson
import Data.Char
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import "unordered-containers" Data.HashSet (HashSet)
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
import Data.Time
import Data.Word
import GHC.Generics

import Data.Swagger.Internal
import Data.Swagger.Internal.ParamSchema (ToParamSchema(..))
import Data.Swagger.Lens
import Data.Swagger.SchemaOptions

-- | A @'Schema'@ with an optional name.
-- This name can be used in references.
type NamedSchema = (Maybe String, Schema)

-- | Schema definitions, a mapping from schema name to @'Schema'@.
type Definitions = HashMap T.Text Schema

unnamed :: Schema -> NamedSchema
unnamed schema = (Nothing, schema)

named :: String -> Schema -> NamedSchema
named name schema = (Just name, schema)

plain :: Schema -> (Definitions, NamedSchema)
plain schema = (mempty, unnamed schema)

unname :: NamedSchema -> NamedSchema
unname (_, schema) = (Nothing, schema)

-- | Convert a type into @'Schema'@.
--
-- An example type and instance:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}   -- allows to write 'T.Text' literals
-- {-\# LANGUAGE OverloadedLists \#-}     -- allows to write 'Map' and 'HashMap' as lists
--
-- import Control.Lens
--
-- data Coord = Coord { x :: Double, y :: Double }
--
-- instance ToSchema Coord where
--   toSchemaDefinitions = pure (Just "Coord", schema)
--    where
--      schema = mempty
--        & schemaType .~ SwaggerObject
--        & schemaProperties .~
--            [ ("x", toSchemaRef (Proxy :: Proxy Double))
--            , ("y", toSchemaRef (Proxy :: Proxy Double))
--            ]
--        & schemaRequired .~ [ "x", "y" ]
-- @
--
-- Instead of manually writing your @'ToSchema'@ instance you can
-- use a default generic implementation of @'toSchemaDefinitions'@.
--
-- To do that, simply add @deriving 'Generic'@ clause to your datatype
-- and declare a @'ToSchema'@ instance for your datatype without
-- giving definition for @'toSchemaDefinitions'@.
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
  -- | Convert a type into an optionally named schema
  -- together with all used definitions.
  -- Note that the schema itself is included in definitions
  -- only if it is recursive (and thus needs its definition in scope).
  toSchemaDefinitions :: proxy a -> (Definitions, NamedSchema)
  default toSchemaDefinitions :: (Generic a, GToSchema (Rep a)) => proxy a -> (Definitions, NamedSchema)
  toSchemaDefinitions = genericToSchemaDefinitions defaultSchemaOptions

-- | Convert a type into an optionally named schema.
toNamedSchema :: ToSchema a => proxy a -> NamedSchema
toNamedSchema = snd . toSchemaDefinitions

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
  gtoSchemaDefinitions :: SchemaOptions -> proxy f -> (Definitions, Schema) -> (Definitions, NamedSchema)

gtoNamedSchema :: GToSchema f => SchemaOptions -> proxy f -> (Definitions, Schema) -> NamedSchema
gtoNamedSchema opts proxy = snd . gtoSchemaDefinitions opts proxy

gtoSchema :: GToSchema f => SchemaOptions -> proxy f -> (Definitions, Schema) -> Schema
gtoSchema opts proxy = snd . gtoNamedSchema opts proxy

instance {-# OVERLAPPABLE #-} ToSchema a => ToSchema [a] where
  toSchemaDefinitions _ = (defs, unnamed schema)
    where
      schema = mempty
        & schemaType  .~ SwaggerArray
        & schemaItems ?~ SchemaItemsObject (toSchemaRef (Proxy :: Proxy a))

      (defs, _) = toSchemaDefinitions (Proxy :: Proxy a)

instance {-# OVERLAPPING #-} ToSchema String where toSchemaDefinitions = plain . paramSchemaToSchema
instance ToSchema Bool    where toSchemaDefinitions = plain . paramSchemaToSchema
instance ToSchema Integer where toSchemaDefinitions = plain . paramSchemaToSchema
instance ToSchema Int     where toSchemaDefinitions = plain . paramSchemaToSchema
instance ToSchema Int8    where toSchemaDefinitions = plain . paramSchemaToSchema
instance ToSchema Int16   where toSchemaDefinitions = plain . paramSchemaToSchema
instance ToSchema Int32   where toSchemaDefinitions = plain . paramSchemaToSchema
instance ToSchema Int64   where toSchemaDefinitions = plain . paramSchemaToSchema
instance ToSchema Word    where toSchemaDefinitions = plain . paramSchemaToSchema
instance ToSchema Word8   where toSchemaDefinitions = plain . paramSchemaToSchema
instance ToSchema Word16  where toSchemaDefinitions = plain . paramSchemaToSchema
instance ToSchema Word32  where toSchemaDefinitions = plain . paramSchemaToSchema
instance ToSchema Word64  where toSchemaDefinitions = plain . paramSchemaToSchema

instance ToSchema Char        where toSchemaDefinitions = plain . paramSchemaToSchema
instance ToSchema Scientific  where toSchemaDefinitions = plain . paramSchemaToSchema
instance ToSchema Double      where toSchemaDefinitions = plain . paramSchemaToSchema
instance ToSchema Float       where toSchemaDefinitions = plain . paramSchemaToSchema

instance ToSchema a => ToSchema (Maybe a) where
  toSchemaDefinitions _ = (defs, unnamed schema)
    where
      (defs, (_, schema)) = toSchemaDefinitions (Proxy :: Proxy a)

instance (ToSchema a, ToSchema b) => ToSchema (Either a b)

instance ToSchema ()
instance (ToSchema a, ToSchema b) => ToSchema (a, b)
instance (ToSchema a, ToSchema b, ToSchema c) => ToSchema (a, b, c)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d) => ToSchema (a, b, c, d)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d, ToSchema e) => ToSchema (a, b, c, d, e)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d, ToSchema e, ToSchema f) => ToSchema (a, b, c, d, e, f)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d, ToSchema e, ToSchema f, ToSchema g) => ToSchema (a, b, c, d, e, f, g)

timeSchemaDefinitions :: String -> String -> (Definitions, NamedSchema)
timeSchemaDefinitions name format = pure (Just name, mempty
  & schemaType .~ SwaggerString
  & schemaFormat ?~ T.pack format
  & schemaMinLength ?~ toInteger (length format))

-- |
-- >>> toSchema (Proxy :: Proxy Day) ^. schemaFormat
-- Just "yyyy-mm-dd"
instance ToSchema Day where
  toSchemaDefinitions _ = timeSchemaDefinitions "Day" "yyyy-mm-dd"

-- |
-- >>> toSchema (Proxy :: Proxy LocalTime) ^. schemaFormat
-- Just "yyyy-mm-ddThh:MM:ss"
instance ToSchema LocalTime where
  toSchemaDefinitions _ = timeSchemaDefinitions "LocalTime" "yyyy-mm-ddThh:MM:ss"

-- |
-- >>> toSchema (Proxy :: Proxy ZonedTime) ^. schemaFormat
-- Just "yyyy-mm-ddThh:MM:ss(Z|+hh:MM)"
instance ToSchema ZonedTime where
  toSchemaDefinitions _ = pure (Just "ZonedTime", mempty
    & schemaType .~ SwaggerString
    & schemaFormat ?~ "yyyy-mm-ddThh:MM:ss(Z|+hh:MM)"
    & schemaMinLength ?~ toInteger (length ("yyyy-mm-ddThh:MM:ssZ" :: String)))

instance ToSchema NominalDiffTime where
  toSchemaDefinitions _ = toSchemaDefinitions (Proxy :: Proxy Integer)

-- |
-- >>> toSchema (Proxy :: Proxy UTCTime) ^. schemaFormat
-- Just "yyyy-mm-ddThh:MM:ssZ"
instance ToSchema UTCTime where
  toSchemaDefinitions _ = timeSchemaDefinitions "UTCTime" "yyyy-mm-ddThh:MM:ssZ"

instance ToSchema T.Text where toSchemaDefinitions = plain . paramSchemaToSchema
instance ToSchema TL.Text where toSchemaDefinitions = plain . paramSchemaToSchema

instance ToSchema IntSet where toSchemaDefinitions _ = toSchemaDefinitions (Proxy :: Proxy (Set Int))

-- | NOTE: This schema does not account for the uniqueness of keys.
instance ToSchema a => ToSchema (IntMap a) where
  toSchemaDefinitions _ = toSchemaDefinitions (Proxy :: Proxy [(Int, a)])

instance ToSchema a => ToSchema (Map String a) where
  toSchemaDefinitions _ = (defs, unnamed schema)
    where
      schema = mempty
        & schemaType  .~ SwaggerObject
        & schemaAdditionalProperties ?~ toSchema (Proxy :: Proxy a)

      (defs, _) = toSchemaDefinitions (Proxy :: Proxy a)

instance ToSchema a => ToSchema (Map T.Text  a) where toSchemaDefinitions _ = toSchemaDefinitions (Proxy :: Proxy (Map String a))
instance ToSchema a => ToSchema (Map TL.Text a) where toSchemaDefinitions _ = toSchemaDefinitions (Proxy :: Proxy (Map String a))

instance ToSchema a => ToSchema (HashMap String  a) where toSchemaDefinitions _ = toSchemaDefinitions (Proxy :: Proxy (Map String a))
instance ToSchema a => ToSchema (HashMap T.Text  a) where toSchemaDefinitions _ = toSchemaDefinitions (Proxy :: Proxy (Map String a))
instance ToSchema a => ToSchema (HashMap TL.Text a) where toSchemaDefinitions _ = toSchemaDefinitions (Proxy :: Proxy (Map String a))

instance ToSchema a => ToSchema (Set a) where
  toSchemaDefinitions _ = (defs, unnamed (schema & schemaUniqueItems ?~ True))
    where
      (defs, (_, schema)) = toSchemaDefinitions (Proxy :: Proxy [a])

instance ToSchema a => ToSchema (HashSet a) where toSchemaDefinitions _ = toSchemaDefinitions (Proxy :: Proxy (Set a))

instance ToSchema All where toSchemaDefinitions = plain . paramSchemaToSchema
instance ToSchema Any where toSchemaDefinitions = plain . paramSchemaToSchema

instance ToSchema a => ToSchema (Sum a)     where toSchemaDefinitions _ = unname <$> toSchemaDefinitions (Proxy :: Proxy a)
instance ToSchema a => ToSchema (Product a) where toSchemaDefinitions _ = unname <$> toSchemaDefinitions (Proxy :: Proxy a)
instance ToSchema a => ToSchema (First a)   where toSchemaDefinitions _ = unname <$> toSchemaDefinitions (Proxy :: Proxy a)
instance ToSchema a => ToSchema (Last a)    where toSchemaDefinitions _ = unname <$> toSchemaDefinitions (Proxy :: Proxy a)
instance ToSchema a => ToSchema (Dual a)    where toSchemaDefinitions _ = unname <$> toSchemaDefinitions (Proxy :: Proxy a)

-- | Default schema for @'Bounded'@, @'Integral'@ types.
toSchemaBoundedIntegral :: forall a proxy. (Bounded a, Integral a) => proxy a -> Schema
toSchemaBoundedIntegral _ = mempty
  & schemaType .~ SwaggerInteger
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
genericToNamedSchema :: (Generic a, GToSchema (Rep a)) => SchemaOptions -> proxy a -> NamedSchema
genericToNamedSchema opts = snd . genericToSchemaDefinitions opts

-- | A configurable generic @('NamedSchema', 'Definitions')@ creator.
-- This function applied to @'defaultSchemaOptions'@
-- is used as the default for @'toSchemaDefinitions'@
-- when the type is an instance of @'Generic'@.
genericToSchemaDefinitions :: forall a proxy. (Generic a, GToSchema (Rep a)) => SchemaOptions -> proxy a -> (Definitions, NamedSchema)
genericToSchemaDefinitions opts _ = gtoSchemaDefinitions opts (Proxy :: Proxy (Rep a)) mempty

gdatatypeSchemaName :: forall proxy d. Datatype d => SchemaOptions -> proxy d -> Maybe String
gdatatypeSchemaName opts _ = case name of
  (c:_) | isAlpha c && isUpper c -> Just name
  _ -> Nothing
  where
    name = datatypeNameModifier opts (datatypeName (Proxy3 :: Proxy3 d f a))

-- | Lift a plain @'ParamSchema'@ into a model @'NamedSchema'@.
paramSchemaToNamedSchema :: forall a d f proxy.
  (ToParamSchema a, Generic a, Rep a ~ D1 d f, Datatype d)
  => SchemaOptions -> proxy a -> NamedSchema
paramSchemaToNamedSchema opts proxy = (gdatatypeSchemaName opts (Proxy :: Proxy d), paramSchemaToSchema proxy)

-- | Lift a plain @'ParamSchema'@ into a model @'Schema'@.
paramSchemaToSchema :: forall a proxy. ToParamSchema a => proxy a -> Schema
paramSchemaToSchema _ = mempty & schemaParamSchema .~ toParamSchema (Proxy :: Proxy a)

nullarySchema :: Schema
nullarySchema = mempty
  & schemaType .~ SwaggerArray
  & schemaEnum ?~ [ toJSON () ]

instance GToSchema U1 where
  gtoSchemaDefinitions _ _ _ = plain nullarySchema

instance (GToSchema f, GToSchema g) => GToSchema (f :*: g) where
  gtoSchemaDefinitions opts _ (defs, schema) = (fdefs, unnamed fschema)
    where
      (fdefs, (_, fschema)) = gtoSchemaDefinitions opts (Proxy :: Proxy f) (gdefs, gschema)
      (gdefs, (_, gschema)) = gtoSchemaDefinitions opts (Proxy :: Proxy g) (defs, schema)

instance (Datatype d, GToSchema f) => GToSchema (D1 d f) where
  gtoSchemaDefinitions opts _ (ds, s) = (schemaDefs, (name, schema))
    where
      (schemaDefs, (_, schema)) = gtoSchemaDefinitions opts (Proxy :: Proxy f) (ds, s)
      name = gdatatypeSchemaName opts (Proxy :: Proxy d)

instance {-# OVERLAPPABLE #-} GToSchema f => GToSchema (C1 c f) where
  gtoSchemaDefinitions opts _ (ds, s) = (defs, unnamed schema)
    where
      (defs, (_, schema)) = gtoSchemaDefinitions opts (Proxy :: Proxy f) (ds, s)

-- | Single field constructor.
instance (Selector s, GToSchema f) => GToSchema (C1 c (S1 s f)) where
  gtoSchemaDefinitions opts _ (ds, s)
    | unwrapUnaryRecords opts = (fieldDefs, fieldNamedSchema)
    | otherwise =
        case schema ^. schemaItems of
          Just (SchemaItemsArray [_]) -> (fieldDefs, fieldNamedSchema)
          _ -> (defs, unnamed schema)
    where
      (defs, (_, schema)) = schemaDefs
      schemaDefs = gtoSchemaDefinitions opts (Proxy :: Proxy (S1 s f)) (ds, s)
      (fieldDefs, fieldNamedSchema) = gtoSchemaDefinitions opts (Proxy :: Proxy f) (ds, s)

gtoSchemaRef :: GToSchema f => SchemaOptions -> proxy f -> (Definitions, Referenced Schema)
gtoSchemaRef opts proxy = case gtoSchemaDefinitions opts proxy mempty of
  (defs, (Just name, _))
    | useReferences opts -> (defs, Ref (Reference ("#/definitions/" <> T.pack name)))
  (defs, (_, schema))    -> (defs, Inline schema)

appendItem :: Referenced Schema -> Maybe SchemaItems -> Maybe SchemaItems
appendItem x Nothing = Just (SchemaItemsArray [x])
appendItem x (Just (SchemaItemsArray xs)) = Just (SchemaItemsArray (x:xs))
appendItem _ _ = error "GToSchema.appendItem: cannot append to SchemaItemsObject"

withFieldSchema :: forall proxy s f. (Selector s, GToSchema f) =>
  SchemaOptions -> proxy s f -> Bool -> (Definitions, Schema) -> (Definitions, Schema)
withFieldSchema opts _ isRequiredField (ds, schema) = (ds <> defs, schema')
  where
    schema'
      | T.null fieldName = schema
          & schemaType .~ SwaggerArray
          & schemaItems %~ appendItem fieldSchemaRef
      | otherwise = schema
          & schemaType .~ SwaggerObject
          & schemaProperties . at fieldName ?~ fieldSchemaRef
          & if isRequiredField
              then schemaRequired %~ (fieldName :)
              else id

    fieldName = T.pack (fieldLabelModifier opts (selName (Proxy3 :: Proxy3 s f p)))
    (defs, fieldSchemaRef) = gtoSchemaRef opts (Proxy :: Proxy f)

-- | Optional record fields.
instance {-# OVERLAPPING #-} (Selector s, ToSchema c) => GToSchema (S1 s (K1 i (Maybe c))) where
  gtoSchemaDefinitions opts _ = fmap unnamed . withFieldSchema opts (Proxy2 :: Proxy2 s (K1 i (Maybe c))) False

-- | Record fields.
instance {-# OVERLAPPABLE #-} (Selector s, GToSchema f) => GToSchema (S1 s f) where
  gtoSchemaDefinitions opts _ = fmap unnamed . withFieldSchema opts (Proxy2 :: Proxy2 s f) True

instance ToSchema c => GToSchema (K1 i c) where
  gtoSchemaDefinitions _ _ (ds, _)
    | present   = (ds, (name, schema))
    | otherwise = (ds <> defs <> def, (name, schema))
    where
      (present, def) = case name of
        Nothing -> (False, mempty)
        Just n  -> (HashMap.member (T.pack n) ds, [(T.pack n, schema)])
      (defs, (name, schema)) = toSchemaDefinitions (Proxy :: Proxy c)

instance (GSumToSchema f, GSumToSchema g) => GToSchema (f :+: g) where
  gtoSchemaDefinitions opts _ (ds, s)
    | allNullaryToStringTag opts && allNullary = (ds, unnamed (toStringTag sumSchema))
    | otherwise = (ds <> defs, unnamed sumSchema)
    where
      (All allNullary, (defs, sumSchema)) = gsumToSchema opts (Proxy :: Proxy (f :+: g)) (ds, s)

      toStringTag schema = mempty
        & schemaType .~ SwaggerString
        & schemaEnum ?~ map toJSON (schema ^.. schemaProperties.ifolded.asIndex)

type AllNullary = All

class GSumToSchema f where
  gsumToSchema :: SchemaOptions -> proxy f -> (Definitions, Schema) -> (AllNullary, (Definitions, Schema))

instance (GSumToSchema f, GSumToSchema g) => GSumToSchema (f :+: g) where
  gsumToSchema opts _ = gsumToSchema opts (Proxy :: Proxy f) `after` gsumToSchema opts (Proxy :: Proxy g)
    where
      (f `after` g) s = (a <> b, s'')
        where
          (a, s')  = f s
          (b, s'') = g s'

gsumConToSchema :: forall c f proxy. Constructor c =>
  Bool -> (Definitions, Referenced Schema) -> SchemaOptions -> proxy (C1 c f) -> (Definitions, Schema) -> (AllNullary, (Definitions, Schema))
gsumConToSchema isNullary (tagSchemaDefs, tagSchemaRef) opts _ (defs, schema) = (All isNullary, (defs <> tagSchemaDefs, schema
  & schemaType .~ SwaggerObject
  & schemaProperties . at tag ?~ tagSchemaRef
  & schemaMaxProperties ?~ 1
  & schemaMinProperties ?~ 1))
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

