{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Swagger.Internal.Param where

import Control.Lens
import Data.Aeson
import Data.Proxy
import GHC.Generics

import Data.Int
import Data.Monoid
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
import Data.Word

import Data.Swagger.Internal
import Data.Swagger.Internal.Schema (SchemaOptions(..), defaultSchemaOptions)
import Data.Swagger.Lens

class ToParamSchema a where
  -- | Convert a type into a non-body parameter schema.
  toParamSchema :: proxy a -> ParamLocation -> ParamOtherSchema
  default toParamSchema :: (Generic a, GToParamSchema (Rep a)) => proxy a -> ParamLocation -> ParamOtherSchema
  toParamSchema = genericToParamSchema defaultSchemaOptions

instance {-# OVERLAPPING #-} ToParamSchema String where
  toParamSchema _ loc = mempty
    & paramOtherSchemaIn   .~ loc
    & schemaType .~ ParamString

instance ToParamSchema Bool where
  toParamSchema _ loc = mempty
    & paramOtherSchemaIn   .~ loc
    & schemaType .~ ParamBoolean

instance ToParamSchema Integer where
  toParamSchema _ loc = mempty
    & paramOtherSchemaIn   .~ loc
    & schemaType .~ ParamInteger

instance ToParamSchema Int    where toParamSchema = toParamSchemaBoundedIntegral
instance ToParamSchema Int8   where toParamSchema = toParamSchemaBoundedIntegral
instance ToParamSchema Int16  where toParamSchema = toParamSchemaBoundedIntegral
instance ToParamSchema Int32  where toParamSchema = toParamSchemaBoundedIntegral
instance ToParamSchema Int64  where toParamSchema = toParamSchemaBoundedIntegral

instance ToParamSchema Word   where toParamSchema = toParamSchemaBoundedIntegral
instance ToParamSchema Word8  where toParamSchema = toParamSchemaBoundedIntegral
instance ToParamSchema Word16 where toParamSchema = toParamSchemaBoundedIntegral
instance ToParamSchema Word32 where toParamSchema = toParamSchemaBoundedIntegral
instance ToParamSchema Word64 where toParamSchema = toParamSchemaBoundedIntegral

toParamSchemaBoundedIntegral :: forall proxy a. (Bounded a, Integral a) => proxy a -> ParamLocation -> ParamOtherSchema
toParamSchemaBoundedIntegral _ loc = mempty
  & paramOtherSchemaIn   .~ loc
  & schemaType .~ ParamInteger
  & schemaMinimum ?~ fromInteger (toInteger (minBound :: a))
  & schemaMaximum ?~ fromInteger (toInteger (maxBound :: a))

instance ToParamSchema Char where
  toParamSchema _ loc = mempty
    & paramOtherSchemaIn   .~ loc
    & schemaType .~ ParamString
    & schemaMaxLength ?~ 1
    & schemaMinLength ?~ 1

instance ToParamSchema Scientific where
  toParamSchema _ loc = mempty
    & paramOtherSchemaIn   .~ loc
    & schemaType .~ ParamNumber

instance ToParamSchema Double where
  toParamSchema _ loc = mempty
    & paramOtherSchemaIn   .~ loc
    & schemaType .~ ParamNumber

instance ToParamSchema Float where
  toParamSchema _ loc = mempty
    & paramOtherSchemaIn   .~ loc
    & schemaType .~ ParamNumber

timeParamSchema :: String -> ParamLocation -> ParamOtherSchema
timeParamSchema format loc = mempty
  & paramOtherSchemaIn      .~ loc
  & schemaType    .~ ParamString
  & schemaFormat  ?~ T.pack format
  & schemaMinLength         ?~ toInteger (length format)

-- |
-- >>> toParamSchema (Proxy :: Proxy Day) ParamQuery ^. schemaFormat
-- Just "yyyy-mm-dd"
instance ToParamSchema Day where
  toParamSchema _ = timeParamSchema "yyyy-mm-dd"

-- |
-- >>> toParamSchema (Proxy :: Proxy LocalTime) ParamQuery ^. schemaFormat
-- Just "yyyy-mm-ddThh:MM:ss"
instance ToParamSchema LocalTime where
  toParamSchema _ = timeParamSchema "yyyy-mm-ddThh:MM:ss"

-- |
-- >>> toParamSchema (Proxy :: Proxy ZonedTime) ParamQuery ^. schemaFormat
-- Just "yyyy-mm-ddThh:MM:ss+hhMM"
instance ToParamSchema ZonedTime where
  toParamSchema _ = timeParamSchema "yyyy-mm-ddThh:MM:ss+hhMM"

-- |
-- >>> toParamSchema (Proxy :: Proxy UTCTime) ParamQuery ^. schemaFormat
-- Just "yyyy-mm-ddThh:MM:ssZ"
instance ToParamSchema UTCTime where
  toParamSchema _ = timeParamSchema "yyyy-mm-ddThh:MM:ssZ"

instance ToParamSchema NominalDiffTime where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Integer)

instance ToParamSchema T.Text where
  toParamSchema _ = toParamSchema (Proxy :: Proxy String)

instance ToParamSchema TL.Text where
  toParamSchema _ = toParamSchema (Proxy :: Proxy String)

instance ToParamSchema All where toParamSchema _ = toParamSchema (Proxy :: Proxy Bool)
instance ToParamSchema Any where toParamSchema _ = toParamSchema (Proxy :: Proxy Bool)
instance ToParamSchema a => ToParamSchema (Sum a)     where toParamSchema _ = toParamSchema (Proxy :: Proxy a)
instance ToParamSchema a => ToParamSchema (Product a) where toParamSchema _ = toParamSchema (Proxy :: Proxy a)
instance ToParamSchema a => ToParamSchema (First a)   where toParamSchema _ = toParamSchema (Proxy :: Proxy a)
instance ToParamSchema a => ToParamSchema (Last a)    where toParamSchema _ = toParamSchema (Proxy :: Proxy a)
instance ToParamSchema a => ToParamSchema (Dual a)    where toParamSchema _ = toParamSchema (Proxy :: Proxy a)

genericToParamSchema :: forall proxy a. (Generic a, GToParamSchema (Rep a)) => SchemaOptions -> proxy a -> ParamLocation -> ParamOtherSchema
genericToParamSchema opts _ loc = gtoParamSchema opts (Proxy :: Proxy (Rep a)) loc mempty

class GToParamSchema (f :: * -> *) where
  gtoParamSchema :: SchemaOptions -> proxy f -> ParamLocation -> ParamOtherSchema -> ParamOtherSchema

instance GToParamSchema f => GToParamSchema (D1 d f) where
  gtoParamSchema opts _ = gtoParamSchema opts (Proxy :: Proxy f)

instance GToParamSchema f => GToParamSchema (C1 c (S1 s f)) where
  gtoParamSchema opts _ = gtoParamSchema opts (Proxy :: Proxy f)

instance ToParamSchema c => GToParamSchema (K1 i c) where
  gtoParamSchema _ _ loc _ = toParamSchema (Proxy :: Proxy c) loc

instance (GEnumParamSchema f, GEnumParamSchema g) => GToParamSchema (f :+: g) where
  gtoParamSchema opts _ = genumParamSchema opts (Proxy :: Proxy (f :+: g))

class GEnumParamSchema (f :: * -> *) where
  genumParamSchema :: SchemaOptions -> proxy f -> ParamLocation -> ParamOtherSchema -> ParamOtherSchema

instance (GEnumParamSchema f, GEnumParamSchema g) => GEnumParamSchema (f :+: g) where
  genumParamSchema opts _ loc = genumParamSchema opts (Proxy :: Proxy f) loc . genumParamSchema opts (Proxy :: Proxy g) loc

instance Constructor c => GEnumParamSchema (C1 c U1) where
  genumParamSchema opts _ loc s = s
    & paramOtherSchemaIn    .~ loc
    & schemaType  .~ ParamString
    & schemaEnum            %~ addEnumValue tag
    where
      tag = toJSON (constructorTagModifier opts (conName (Proxy3 :: Proxy3 c f p)))

      addEnumValue x Nothing    = Just [x]
      addEnumValue x (Just xs)  = Just (x:xs)

data Proxy3 a b c = Proxy3

