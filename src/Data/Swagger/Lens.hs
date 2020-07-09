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
makeFields ''Components
makeFields ''Server
-- conflict with enum of ParamSchema
--makeLensesWith swaggerFieldRules ''ServerVariable
makeFields ''RequestBody
makeFields ''MediaTypeObject
makeFields ''Info
makeFields ''Contact
makeFields ''License
makeLensesWith swaggerFieldRules ''PathItem
makeFields ''Tag
makeFields ''Operation
makeLensesWith swaggerFieldRules ''Param
makeFields ''Header
makeLensesWith swaggerFieldRules ''Schema
makeFields ''NamedSchema
makeLensesWith swaggerFieldRules ''ParamSchema
makeFields ''Xml
makeLensesWith swaggerFieldRules ''Responses
makeFields ''Response
makeLensesWith swaggerFieldRules ''SecurityScheme
makeFields ''ApiKeyParams
makeFields ''OAuth2ImplicitFlow
makeFields ''OAuth2PasswordFlow
makeFields ''OAuth2ClientCredentialsFlow
makeFields ''OAuth2AuthorizationCodeFlow
makeFields ''OAuth2Flow
makeFields ''OAuth2Flows
makeFields ''ExternalDocs
makeFields ''Encoding
makeFields ''Example
makeFields ''Discriminator

-- * Prisms
-- ** 'SecuritySchemeType' prisms
makePrisms ''SecuritySchemeType
-- ** 'Referenced' prisms
makePrisms ''Referenced

-- ** 'SwaggerItems' prisms

_SwaggerItemsArray :: Review SwaggerItems [Referenced Schema]
_SwaggerItemsArray
  = unto (\x -> SwaggerItemsArray x)
{- \x -> case x of
      SwaggerItemsPrimitive c p -> Left (SwaggerItemsPrimitive c p)
      SwaggerItemsObject o      -> Left (SwaggerItemsObject o)
      SwaggerItemsArray a       -> Right a
-}

_SwaggerItemsObject :: Review SwaggerItems (Referenced Schema)
_SwaggerItemsObject
  = unto (\x -> SwaggerItemsObject x)
{- \x -> case x of
      SwaggerItemsPrimitive c p -> Left (SwaggerItemsPrimitive c p)
      SwaggerItemsObject o      -> Right o
      SwaggerItemsArray a       -> Left (SwaggerItemsArray a)
-}

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

instance HasParamSchema NamedSchema ParamSchema where paramSchema = schema.paramSchema

-- HasType instances
instance HasType Schema (Maybe SwaggerType) where type_ = paramSchema.type_
instance HasType NamedSchema (Maybe SwaggerType) where type_ = paramSchema.type_

-- HasDefault instances
instance HasDefault Schema (Maybe Value) where default_ = paramSchema.default_

-- OVERLAPPABLE instances

instance
  {-# OVERLAPPABLE #-}
  HasParamSchema s ParamSchema
  => HasFormat s (Maybe Format) where
  format = paramSchema.format

instance
  {-# OVERLAPPABLE #-}
  HasParamSchema s ParamSchema
  => HasItems s (Maybe SwaggerItems) where
  items = paramSchema.items

instance
  {-# OVERLAPPABLE #-}
  HasParamSchema s ParamSchema
  => HasMaximum s (Maybe Scientific) where
  maximum_ = paramSchema.maximum_

instance {-# OVERLAPPABLE #-} HasParamSchema s ParamSchema
  => HasExclusiveMaximum s (Maybe Bool) where
  exclusiveMaximum = paramSchema.exclusiveMaximum

instance {-# OVERLAPPABLE #-} HasParamSchema s ParamSchema
  => HasMinimum s (Maybe Scientific) where
  minimum_ = paramSchema.minimum_

instance {-# OVERLAPPABLE #-} HasParamSchema s ParamSchema
  => HasExclusiveMinimum s (Maybe Bool) where
  exclusiveMinimum = paramSchema.exclusiveMinimum

instance {-# OVERLAPPABLE #-} HasParamSchema s ParamSchema
  => HasMaxLength s (Maybe Integer) where
  maxLength = paramSchema.maxLength

instance {-# OVERLAPPABLE #-} HasParamSchema s ParamSchema
  => HasMinLength s (Maybe Integer) where
  minLength = paramSchema.minLength

instance {-# OVERLAPPABLE #-} HasParamSchema s ParamSchema
  => HasPattern s (Maybe Text) where
  pattern = paramSchema.pattern

instance {-# OVERLAPPABLE #-} HasParamSchema s ParamSchema
  => HasMaxItems s (Maybe Integer) where
  maxItems = paramSchema.maxItems

instance {-# OVERLAPPABLE #-} HasParamSchema s ParamSchema
  => HasMinItems s (Maybe Integer) where
  minItems = paramSchema.minItems

instance {-# OVERLAPPABLE #-} HasParamSchema s ParamSchema
  => HasUniqueItems s (Maybe Bool) where
  uniqueItems = paramSchema.uniqueItems

instance {-# OVERLAPPABLE #-} HasParamSchema s ParamSchema
  => HasEnum s (Maybe [Value]) where
  enum_ = paramSchema.enum_

instance {-# OVERLAPPABLE #-} HasParamSchema s ParamSchema
  => HasMultipleOf s (Maybe Scientific) where
  multipleOf = paramSchema.multipleOf
