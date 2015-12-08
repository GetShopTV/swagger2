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

import Data.Swagger.Internal
import Data.Swagger.Internal.Schema (SchemaOptions(..), defaultSchemaOptions)
import Data.Swagger.Lens

class ToParamSchema a where
  -- | Convert a type into a non-body parameter schema.
  toParamSchema :: proxy a -> ParamLocation -> ParamOtherSchema
  default toParamSchema :: (Generic a, GToParamSchema (Rep a)) => proxy a -> ParamLocation -> ParamOtherSchema
  toParamSchema = genericToParamSchema defaultSchemaOptions

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
    & paramOtherSchemaType  .~ ParamString
    & schemaEnum            %~ addEnumValue tag
    where
      tag = toJSON (constructorTagModifier opts (conName (Proxy3 :: Proxy3 c f p)))

      addEnumValue x Nothing    = Just [x]
      addEnumValue x (Just xs)  = Just (x:xs)

data Proxy3 a b c = Proxy3

