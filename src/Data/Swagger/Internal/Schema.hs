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
import Data.Data.Lens (template)

import Control.Monad
import Control.Monad.Writer
import Data.Aeson
import Data.Char
import Data.Data (Data)
import Data.Foldable (traverse_)
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

import Data.Swagger.Declare
import Data.Swagger.Internal
import Data.Swagger.Internal.ParamSchema (ToParamSchema(..))
import Data.Swagger.Lens
import Data.Swagger.SchemaOptions

-- | A @'Schema'@ with an optional name.
-- This name can be used in references.
type NamedSchema = (Maybe T.Text, Schema)

-- | Schema definitions, a mapping from schema name to @'Schema'@.
type Definitions = HashMap T.Text Schema

unnamed :: Schema -> NamedSchema
unnamed schema = (Nothing, schema)

named :: T.Text -> Schema -> NamedSchema
named name schema = (Just name, schema)

plain :: Schema -> Declare Definitions NamedSchema
plain = pure . unnamed

unname :: NamedSchema -> NamedSchema
unname (_, schema) = (Nothing, schema)

rename :: Maybe T.Text -> NamedSchema -> NamedSchema
rename name (_, schema) = (name, schema)

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
--   declareNamedSchema = pure (Just \"Coord\", schema)
--    where
--      schema = mempty
--        & schemaType .~ SwaggerObject
--        & schemaProperties .~
--            [ (\"x\", toSchemaRef (Proxy :: Proxy Double))
--            , (\"y\", toSchemaRef (Proxy :: Proxy Double))
--            ]
--        & schemaRequired .~ [ \"x\", \"y\" ]
-- @
--
-- Instead of manually writing your @'ToSchema'@ instance you can
-- use a default generic implementation of @'declareNamedSchema'@.
--
-- To do that, simply add @deriving 'Generic'@ clause to your datatype
-- and declare a @'ToSchema'@ instance for your datatype without
-- giving definition for @'declareNamedSchema'@.
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
  declareNamedSchema :: proxy a -> Declare Definitions NamedSchema
  default declareNamedSchema :: (Generic a, GToSchema (Rep a)) => proxy a -> Declare Definitions NamedSchema
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

-- | Convert a type into a schema and declare all used schema definitions.
declareSchema :: ToSchema a => proxy a -> Declare Definitions Schema
declareSchema = fmap snd . declareNamedSchema

-- | Convert a type into an optionally named schema.
--
-- >>> encode <$> toNamedSchema (Proxy :: Proxy String)
-- (Nothing,"{\"type\":\"string\"}")
--
-- >>> encode <$> toNamedSchema (Proxy :: Proxy Day)
-- (Just "Day","{\"format\":\"date\",\"type\":\"string\"}")
toNamedSchema :: ToSchema a => proxy a -> NamedSchema
toNamedSchema = undeclare . declareNamedSchema

-- | Get type's schema name according to its @'ToSchema'@ instance.
--
-- >>> schemaName (Proxy :: Proxy Int)
-- Nothing
--
-- >>> schemaName (Proxy :: Proxy UTCTime)
-- Just "UTCTime"
schemaName :: ToSchema a => proxy a -> Maybe T.Text
schemaName = fst . toNamedSchema

-- | Convert a type into a schema.
--
-- >>> encode $ toSchema (Proxy :: Proxy Int8)
-- "{\"maximum\":127,\"minimum\":-128,\"type\":\"integer\"}"
--
-- >>> encode $ toSchema (Proxy :: Proxy [Day])
-- "{\"items\":{\"$ref\":\"#/definitions/Day\"},\"type\":\"array\"}"
toSchema :: ToSchema a => proxy a -> Schema
toSchema = snd . toNamedSchema

-- | Convert a type into a referenced schema if possible.
-- Only named schemas can be referenced, nameless schemas are inlined.
--
-- >>> encode $ toSchemaRef (Proxy :: Proxy Integer)
-- "{\"type\":\"integer\"}"
--
-- >>> encode $ toSchemaRef (Proxy :: Proxy Day)
-- "{\"$ref\":\"#/definitions/Day\"}"
toSchemaRef :: ToSchema a => proxy a -> Referenced Schema
toSchemaRef = undeclare . declareSchemaRef

-- | Convert a type into a referenced schema if possible
-- and declare all used schema definitions.
-- Only named schemas can be referenced, nameless schemas are inlined.
--
-- Schema definitions are typically declared for every referenced schema.
-- If @'declareSchemaRef'@ returns a reference, a corresponding schema
-- will be declared (regardless of whether it is recusive or not).
declareSchemaRef :: ToSchema a => proxy a -> Declare Definitions (Referenced Schema)
declareSchemaRef proxy = do
  case toNamedSchema proxy of
    (Just name, schema) -> do
      -- This check is very important as it allows generically
      -- derive used definitions for recursive schemas.
      -- Lazy Declare monad allows toNamedSchema to ignore
      -- any declarations (which would otherwise loop) and
      -- retrieve the schema and its name to check if we
      -- have already declared it.
      -- If we have, we don't need to declare anything for
      -- this schema this time and thus simply return the reference.
      known <- looks (HashMap.member name)
      when (not known) $ do
        declare [(name, schema)]
        void $ declareNamedSchema proxy
      return $ Ref (Reference name)
    _ -> Inline <$> declareSchema proxy

-- | Inline any referenced schema if its name satisfies given predicate.
--
-- /NOTE:/ if a referenced schema is not found in definitions the predicate is ignored
-- and schema stays referenced.
--
-- __WARNING:__ @'inlineSchemasWhen'@ will produce infinite schemas
-- when inlining recursive schemas.
inlineSchemasWhen :: Data s => (T.Text -> Bool) -> Definitions -> s -> s
inlineSchemasWhen p defs = template %~ deref
  where
    deref r@(Ref (Reference name))
      | p name =
          case HashMap.lookup name defs of
            Just schema -> Inline (inlineSchemasWhen p defs schema)
            Nothing -> r
      | otherwise = r
    deref (Inline schema) = Inline (inlineSchemasWhen p defs schema)

-- | Inline any referenced schema if its name is in the given list.
--
-- /NOTE:/ if a referenced schema is not found in definitions
-- it stays referenced even if it appears in the list of names.
--
-- __WARNING:__ @'inlineSchemas'@ will produce infinite schemas
-- when inlining recursive schemas.
inlineSchemas :: Data s => [T.Text] -> Definitions -> s -> s
inlineSchemas names = inlineSchemasWhen (`elem` names)

-- | Inline all schema references for which the definition
-- can be found in @'Definitions'@.
--
-- __WARNING:__ @'inlineAllSchemas'@ will produce infinite schemas
-- when inlining recursive schemas.
inlineAllSchemas :: Data s => Definitions -> s -> s
inlineAllSchemas = inlineSchemasWhen (const True)

-- | Convert a type into a schema without references.
--
-- >>> encode $ toInlinedSchema (Proxy :: Proxy [Day])
-- "{\"items\":{\"format\":\"date\",\"type\":\"string\"},\"type\":\"array\"}"
--
-- __WARNING:__ @'toInlinedSchema'@ will produce infinite schema
-- when inlining recursive schemas.
toInlinedSchema :: ToSchema a => proxy a -> Schema
toInlinedSchema proxy = inlineAllSchemas defs schema
  where
    (defs, schema) = runDeclare (declareSchema proxy) mempty

-- | Inline all /non-recursive/ schemas for which the definition
-- can be found in @'Definitions'@.
inlineNonRecursiveSchemas :: Data s => Definitions -> s -> s
inlineNonRecursiveSchemas defs = inlineSchemasWhen nonRecursive defs
  where
    nonRecursive name =
      case HashMap.lookup name defs of
        Just schema -> name `notElem` execDeclare (usedNames schema) mempty
        Nothing     -> False

    usedNames schema = traverse_ schemaRefNames (schema ^.. template)

    schemaRefNames :: Referenced Schema -> Declare [T.Text] ()
    schemaRefNames ref = case ref of
      Ref (Reference name) -> do
        seen <- looks (name `elem`)
        when (not seen) $ do
          declare [name]
          traverse_ usedNames (HashMap.lookup name defs)
      Inline subschema -> usedNames subschema

class GToSchema (f :: * -> *) where
  gdeclareNamedSchema :: SchemaOptions -> proxy f -> Schema -> Declare Definitions NamedSchema

instance {-# OVERLAPPABLE #-} ToSchema a => ToSchema [a] where
  declareNamedSchema _ = do
    ref <- declareSchemaRef (Proxy :: Proxy a)
    return $ unnamed $ mempty
      & schemaType  .~ SwaggerArray
      & schemaItems ?~ SwaggerItemsObject ref

instance {-# OVERLAPPING #-} ToSchema String where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Bool    where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Integer where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Int     where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Int8    where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Int16   where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Int32   where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Int64   where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Word    where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Word8   where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Word16  where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Word32  where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Word64  where declareNamedSchema = plain . paramSchemaToSchema

instance ToSchema Char        where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Scientific  where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Double      where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Float       where declareNamedSchema = plain . paramSchemaToSchema

instance ToSchema a => ToSchema (Maybe a) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy a)

instance (ToSchema a, ToSchema b) => ToSchema (Either a b)

instance ToSchema () where
  declareNamedSchema _ = pure (Nothing, nullarySchema)

instance (ToSchema a, ToSchema b) => ToSchema (a, b)
instance (ToSchema a, ToSchema b, ToSchema c) => ToSchema (a, b, c)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d) => ToSchema (a, b, c, d)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d, ToSchema e) => ToSchema (a, b, c, d, e)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d, ToSchema e, ToSchema f) => ToSchema (a, b, c, d, e, f)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d, ToSchema e, ToSchema f, ToSchema g) => ToSchema (a, b, c, d, e, f, g)

timeSchema :: T.Text -> Schema
timeSchema format = mempty
  & schemaType .~ SwaggerString
  & schemaFormat ?~ format

-- | Format @"date"@ corresponds to @yyyy-mm-dd@ format.
instance ToSchema Day where
  declareNamedSchema _ = pure $ named "Day" (timeSchema "date")

-- |
-- >>> toSchema (Proxy :: Proxy LocalTime) ^. schemaFormat
-- Just "yyyy-mm-ddThh:MM:ss"
instance ToSchema LocalTime where
  declareNamedSchema _ = pure $ named "LocalTime" (timeSchema "yyyy-mm-ddThh:MM:ss")

-- | Format @"date"@ corresponds to @yyyy-mm-ddThh:MM:ss(Z|+hh:MM)@ format.
instance ToSchema ZonedTime where
  declareNamedSchema _ = pure $ named "ZonedTime" $ timeSchema "date-time"

instance ToSchema NominalDiffTime where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Integer)

-- |
-- >>> toSchema (Proxy :: Proxy UTCTime) ^. schemaFormat
-- Just "yyyy-mm-ddThh:MM:ssZ"
instance ToSchema UTCTime where
  declareNamedSchema _ = pure $ named "UTCTime" (timeSchema "yyyy-mm-ddThh:MM:ssZ")

instance ToSchema T.Text where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema TL.Text where declareNamedSchema = plain . paramSchemaToSchema

instance ToSchema IntSet where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Set Int))

-- | NOTE: This schema does not account for the uniqueness of keys.
instance ToSchema a => ToSchema (IntMap a) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy [(Int, a)])

instance ToSchema a => ToSchema (Map String a) where
  declareNamedSchema _ = do
    schema <- declareSchema (Proxy :: Proxy a)
    return $ unnamed $ mempty
      & schemaType  .~ SwaggerObject
      & schemaAdditionalProperties ?~ schema

instance ToSchema a => ToSchema (Map T.Text  a) where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Map String a))
instance ToSchema a => ToSchema (Map TL.Text a) where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Map String a))

instance ToSchema a => ToSchema (HashMap String  a) where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Map String a))
instance ToSchema a => ToSchema (HashMap T.Text  a) where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Map String a))
instance ToSchema a => ToSchema (HashMap TL.Text a) where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Map String a))

instance ToSchema a => ToSchema (Set a) where
  declareNamedSchema _ = do
    schema <- declareSchema (Proxy :: Proxy [a])
    return $ unnamed $ schema
      & schemaUniqueItems ?~ True

instance ToSchema a => ToSchema (HashSet a) where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Set a))

instance ToSchema All where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Any where declareNamedSchema = plain . paramSchemaToSchema

instance ToSchema a => ToSchema (Sum a)     where declareNamedSchema _ = unname <$> declareNamedSchema (Proxy :: Proxy a)
instance ToSchema a => ToSchema (Product a) where declareNamedSchema _ = unname <$> declareNamedSchema (Proxy :: Proxy a)
instance ToSchema a => ToSchema (First a)   where declareNamedSchema _ = unname <$> declareNamedSchema (Proxy :: Proxy a)
instance ToSchema a => ToSchema (Last a)    where declareNamedSchema _ = unname <$> declareNamedSchema (Proxy :: Proxy a)
instance ToSchema a => ToSchema (Dual a)    where declareNamedSchema _ = unname <$> declareNamedSchema (Proxy :: Proxy a)

-- | Default schema for @'Bounded'@, @'Integral'@ types.
--
-- >>> encode $ toSchemaBoundedIntegral (Proxy :: Proxy Int16)
-- "{\"maximum\":32767,\"minimum\":-32768,\"type\":\"integer\"}"
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
genericDeclareSchema :: (Generic a, GToSchema (Rep a)) => SchemaOptions -> proxy a -> Declare Definitions Schema
genericDeclareSchema opts proxy = snd <$> genericDeclareNamedSchema opts proxy

-- | A configurable generic @'NamedSchema'@ creator.
-- This function applied to @'defaultSchemaOptions'@
-- is used as the default for @'declareNamedSchema'@
-- when the type is an instance of @'Generic'@.
genericDeclareNamedSchema :: forall a proxy. (Generic a, GToSchema (Rep a)) => SchemaOptions -> proxy a -> Declare Definitions NamedSchema
genericDeclareNamedSchema opts _ = gdeclareNamedSchema opts (Proxy :: Proxy (Rep a)) mempty

gdatatypeSchemaName :: forall proxy d. Datatype d => SchemaOptions -> proxy d -> Maybe T.Text
gdatatypeSchemaName opts _ = case name of
  (c:_) | isAlpha c && isUpper c -> Just (T.pack name)
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

gtoNamedSchema :: GToSchema f => SchemaOptions -> proxy f -> NamedSchema
gtoNamedSchema opts proxy = undeclare $ gdeclareNamedSchema opts proxy mempty

gdeclareSchema :: GToSchema f => SchemaOptions -> proxy f -> Declare Definitions Schema
gdeclareSchema opts proxy = snd <$> gdeclareNamedSchema opts proxy mempty

instance (GToSchema f, GToSchema g) => GToSchema (f :*: g) where
  gdeclareNamedSchema opts _ schema = do
    (_, gschema) <- gdeclareNamedSchema opts (Proxy :: Proxy g) schema
    gdeclareNamedSchema opts (Proxy :: Proxy f) gschema

instance (Datatype d, GToSchema f) => GToSchema (D1 d f) where
  gdeclareNamedSchema opts _ s = rename name <$> gdeclareNamedSchema opts (Proxy :: Proxy f) s
    where
      name = gdatatypeSchemaName opts (Proxy :: Proxy d)

instance {-# OVERLAPPABLE #-} GToSchema f => GToSchema (C1 c f) where
  gdeclareNamedSchema opts _ = gdeclareNamedSchema opts (Proxy :: Proxy f)

instance {-# OVERLAPPING #-} Constructor c => GToSchema (C1 c U1) where
  gdeclareNamedSchema = gdeclareNamedSumSchema

-- | Single field constructor.
instance (Selector s, GToSchema f) => GToSchema (C1 c (S1 s f)) where
  gdeclareNamedSchema opts _ s
    | unwrapUnaryRecords opts = fieldSchema
    | otherwise = do
        (_, schema) <- recordSchema
        case schema ^. schemaItems of
          Just (SwaggerItemsArray [_]) -> fieldSchema
          _ -> pure (unnamed schema)
    where
      recordSchema = gdeclareNamedSchema opts (Proxy :: Proxy (S1 s f)) s
      fieldSchema  = gdeclareNamedSchema opts (Proxy :: Proxy f) s

gdeclareSchemaRef :: GToSchema a => SchemaOptions -> proxy a -> Declare Definitions (Referenced Schema)
gdeclareSchemaRef opts proxy = do
  case gtoNamedSchema opts proxy of
    (Just name, schema) -> do
      -- This check is very important as it allows generically
      -- derive used definitions for recursive schemas.
      -- Lazy Declare monad allows toNamedSchema to ignore
      -- any declarations (which would otherwise loop) and
      -- retrieve the schema and its name to check if we
      -- have already declared it.
      -- If we have, we don't need to declare anything for
      -- this schema this time and thus simply return the reference.
      known <- looks (HashMap.member name)
      when (not known) $ do
        declare [(name, schema)]
        void $ gdeclareNamedSchema opts proxy mempty
      return $ Ref (Reference name)
    _ -> Inline <$> gdeclareSchema opts proxy

appendItem :: Referenced Schema -> Maybe (SwaggerItems Schema) -> Maybe (SwaggerItems Schema)
appendItem x Nothing = Just (SwaggerItemsArray [x])
appendItem x (Just (SwaggerItemsArray xs)) = Just (SwaggerItemsArray (x:xs))
appendItem _ _ = error "GToSchema.appendItem: cannot append to SwaggerItemsObject"

withFieldSchema :: forall proxy s f. (Selector s, GToSchema f) =>
  SchemaOptions -> proxy s f -> Bool -> Schema -> Declare Definitions Schema
withFieldSchema opts _ isRequiredField schema = do
  ref <- gdeclareSchemaRef opts (Proxy :: Proxy f)
  return $
    if T.null fname
      then schema
        & schemaType .~ SwaggerArray
        & schemaItems %~ appendItem ref
      else schema
        & schemaType .~ SwaggerObject
        & schemaProperties . at fname ?~ ref
        & if isRequiredField
            then schemaRequired %~ (fname :)
            else id
  where
    fname = T.pack (fieldLabelModifier opts (selName (Proxy3 :: Proxy3 s f p)))

-- | Optional record fields.
instance {-# OVERLAPPING #-} (Selector s, ToSchema c) => GToSchema (S1 s (K1 i (Maybe c))) where
  gdeclareNamedSchema opts _ = fmap unnamed . withFieldSchema opts (Proxy2 :: Proxy2 s (K1 i (Maybe c))) False

-- | Record fields.
instance {-# OVERLAPPABLE #-} (Selector s, GToSchema f) => GToSchema (S1 s f) where
  gdeclareNamedSchema opts _ = fmap unnamed . withFieldSchema opts (Proxy2 :: Proxy2 s f) True

instance {-# OVERLAPPING #-} ToSchema c => GToSchema (K1 i (Maybe c)) where
  gdeclareNamedSchema _ _ _ = declareNamedSchema (Proxy :: Proxy c)

instance {-# OVERLAPPABLE #-} ToSchema c => GToSchema (K1 i c) where
  gdeclareNamedSchema _ _ _ = declareNamedSchema (Proxy :: Proxy c)

instance (GSumToSchema f, GSumToSchema g) => GToSchema (f :+: g) where
  gdeclareNamedSchema = gdeclareNamedSumSchema

gdeclareNamedSumSchema :: GSumToSchema f => SchemaOptions -> proxy f -> Schema -> Declare Definitions NamedSchema
gdeclareNamedSumSchema opts proxy s
  | allNullaryToStringTag opts && allNullary = pure $ unnamed (toStringTag sumSchema)
  | otherwise = (unnamed . fst) <$> runWriterT declareSumSchema
  where
    declareSumSchema = gsumToSchema opts proxy s
    (sumSchema, All allNullary) = undeclare (runWriterT declareSumSchema)

    toStringTag schema = mempty
      & schemaType .~ SwaggerString
      & schemaEnum ?~ map toJSON (schema ^.. schemaProperties.ifolded.asIndex)

type AllNullary = All

class GSumToSchema f where
  gsumToSchema :: SchemaOptions -> proxy f -> Schema -> WriterT AllNullary (Declare Definitions) Schema

instance (GSumToSchema f, GSumToSchema g) => GSumToSchema (f :+: g) where
  gsumToSchema opts _ = gsumToSchema opts (Proxy :: Proxy f) <=< gsumToSchema opts (Proxy :: Proxy g)

gsumConToSchema :: forall c f proxy. (GToSchema (C1 c f), Constructor c) =>
  SchemaOptions -> proxy (C1 c f) -> Schema -> Declare Definitions Schema
gsumConToSchema opts _ schema = do
  ref <- gdeclareSchemaRef opts (Proxy :: Proxy (C1 c f))
  return $ schema
    & schemaType .~ SwaggerObject
    & schemaProperties . at tag ?~ ref
    & schemaMaxProperties ?~ 1
    & schemaMinProperties ?~ 1
  where
    tag = T.pack (constructorTagModifier opts (conName (Proxy3 :: Proxy3 c f p)))

instance {-# OVERLAPPABLE #-} (Constructor c, GToSchema f) => GSumToSchema (C1 c f) where
  gsumToSchema opts proxy schema = do
    tell (All False)
    lift $ gsumConToSchema opts proxy schema

instance (Constructor c, Selector s, GToSchema f) => GSumToSchema (C1 c (S1 s f)) where
  gsumToSchema opts proxy schema = do
    tell (All False)
    lift $ gsumConToSchema opts proxy schema

instance Constructor c => GSumToSchema (C1 c U1) where
  gsumToSchema opts proxy = lift . gsumConToSchema opts proxy

data Proxy2 a b = Proxy2

data Proxy3 a b c = Proxy3

