{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#include "overlapping-compat.h"
-- |
-- Module:      Data.Swagger.Lens
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Lenses and prisms for Swagger.
module Data.Swagger.Lens where

import Control.Lens
import Data.Aeson (Value)
import Data.Scientific (Scientific)
import Data.Swagger.Internal
import Data.Swagger.Internal.Utils
import Data.Text (Text)

-- * Classy lenses

makeFields ''Swagger
makeFields ''Host
makeFields ''Info
makeFields ''Contact
makeFields ''License
makeLensesWith swaggerFieldRules ''PathItem
makeFields ''Tag
makeFields ''Operation
makeFields ''Param
makeLensesWith swaggerFieldRules ''ParamOtherSchema
makeFields ''Header
makeFields ''Schema
makeFields ''NamedSchema
makeLensesWith swaggerFieldRules ''ParamSchema
makeFields ''Xml
makeLensesWith swaggerFieldRules ''Responses
makeFields ''Response
makeLensesWith swaggerFieldRules ''SecurityScheme
makeFields ''ApiKeyParams
makeFields ''OAuth2Params
makeFields ''ExternalDocs

-- * Prisms
-- ** 'ParamAnySchema' prisms
makePrisms ''ParamAnySchema
-- ** 'SecuritySchemeType' prisms
makePrisms ''SecuritySchemeType
-- ** 'Referenced' prisms
makePrisms ''Referenced

-- ** 'SwaggerItems' prisms

_SwaggerItemsArray :: Review (SwaggerItems SwaggerKindSchema) [Referenced Schema]
_SwaggerItemsArray
  = prism (\x -> SwaggerItemsArray x) $ \x -> case x of
      SwaggerItemsPrimitive c p -> Left (SwaggerItemsPrimitive c p)
      SwaggerItemsObject o      -> Left (SwaggerItemsObject o)
      SwaggerItemsArray a       -> Right a

_SwaggerItemsObject :: Review (SwaggerItems SwaggerKindSchema) (Referenced Schema)
_SwaggerItemsObject
  = prism (\x -> SwaggerItemsObject x) $ \x -> case x of
      SwaggerItemsPrimitive c p -> Left (SwaggerItemsPrimitive c p)
      SwaggerItemsObject o      -> Right o
      SwaggerItemsArray a       -> Left (SwaggerItemsArray a)

_SwaggerItemsPrimitive :: forall t p f. (Profunctor p, Bifunctor p, Functor f) => Optic' p f (SwaggerItems t) (Maybe (CollectionFormat t), ParamSchema t)
_SwaggerItemsPrimitive = unto (\(c, p) -> SwaggerItemsPrimitive c p)

-- =============================================================
-- More helpful instances for easier access to schema properties

type instance Index Responses = HttpStatusCode
type instance Index Operation = HttpStatusCode

type instance IxValue Responses = Referenced Response
type instance IxValue Operation = Referenced Response

instance Ixed Responses where ix n = responses . ix n
instance At   Responses where at n = responses . at n

instance Ixed Operation where ix n = responses . ix n
instance At   Operation where at n = responses . at n

instance HasParamSchema NamedSchema (ParamSchema SwaggerKindSchema) where paramSchema = schema.paramSchema

-- HasType instances
instance HasType Header (SwaggerType (SwaggerKindNormal Header)) where type_ = paramSchema.type_
instance HasType Schema (SwaggerType SwaggerKindSchema) where type_ = paramSchema.type_
instance HasType NamedSchema (SwaggerType SwaggerKindSchema) where type_ = paramSchema.type_
instance HasType ParamOtherSchema (SwaggerType SwaggerKindParamOtherSchema) where type_ = paramSchema.type_

-- HasDefault instances
instance HasDefault Header (Maybe Value) where default_ = paramSchema.default_
instance HasDefault Schema (Maybe Value) where default_ = paramSchema.default_
instance HasDefault ParamOtherSchema (Maybe Value) where default_ = paramSchema.default_

-- OVERLAPPABLE instances

instance
#if __GLASGOW_HASKELL__ >= 710
  OVERLAPPABLE_
#endif
  HasParamSchema s (ParamSchema t)
  => HasFormat s (Maybe Format) where
  format = paramSchema.format

instance
#if __GLASGOW_HASKELL__ >= 710
  OVERLAPPABLE_
#endif
  HasParamSchema s (ParamSchema t)
  => HasItems s (Maybe (SwaggerItems t)) where
  items = paramSchema.items

instance
#if __GLASGOW_HASKELL__ >= 710
  OVERLAPPABLE_
#endif
  HasParamSchema s (ParamSchema t)
  => HasMaximum s (Maybe Scientific) where
  maximum_ = paramSchema.maximum_

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasExclusiveMaximum s (Maybe Bool) where
  exclusiveMaximum = paramSchema.exclusiveMaximum

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasMinimum s (Maybe Scientific) where
  minimum_ = paramSchema.minimum_

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasExclusiveMinimum s (Maybe Bool) where
  exclusiveMinimum = paramSchema.exclusiveMinimum

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasMaxLength s (Maybe Integer) where
  maxLength = paramSchema.maxLength

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasMinLength s (Maybe Integer) where
  minLength = paramSchema.minLength

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasPattern s (Maybe Text) where
  pattern = paramSchema.pattern

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasMaxItems s (Maybe Integer) where
  maxItems = paramSchema.maxItems

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasMinItems s (Maybe Integer) where
  minItems = paramSchema.minItems

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasUniqueItems s (Maybe Bool) where
  uniqueItems = paramSchema.uniqueItems

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasEnum s (Maybe [Value]) where
  enum_ = paramSchema.enum_

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasMultipleOf s (Maybe Scientific) where
  multipleOf = paramSchema.multipleOf
