{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Swagger.Lens where

import Control.Lens
import Data.Aeson (Value)
import Data.Scientific (Scientific)
import Data.Swagger.Internal
import Data.Swagger.Internal.Utils
import Data.Text (Text)

-- =======================================================================
-- * TH derived lenses

-- ** 'Swagger' lenses
makeFields ''Swagger
-- ** 'Host' lenses
makeFields ''Host
-- ** 'Info' lenses
makeFields ''Info
-- ** 'Contact' lenses
makeFields ''Contact
-- ** 'License' lenses
makeFields ''License
-- ** 'PathItem' lenses
makeFields ''PathItem
-- ** 'Tag' lenses
makeFields ''Tag
-- ** 'Operation' lenses
makeFields ''Operation
-- ** 'Param' lenses
makeFields ''Param
-- ** 'ParamAnySchema' prisms
makePrisms ''ParamAnySchema
-- ** 'ParamOtherSchema' lenses
makeLensesWith swaggerFieldRules ''ParamOtherSchema
-- ** 'Header' lenses
makeFields ''Header
-- ** 'Schema' lenses
makeFields ''Schema
-- ** 'NamedSchema' lenses
makeFields ''NamedSchema

-- ** 'SwaggerItems' prisms

_SwaggerItemsArray :: forall t. (t ~ Schema) => Review (SwaggerItems t) [Referenced Schema]
_SwaggerItemsArray
  = prism (\x -> SwaggerItemsArray x) $ \x -> case x of
      SwaggerItemsPrimitive c p -> Left (SwaggerItemsPrimitive c p)
      SwaggerItemsObject o      -> Left (SwaggerItemsObject o)
      SwaggerItemsArray a       -> Right a

_SwaggerItemsObject :: forall t. (t ~ Schema) => Review (SwaggerItems t) (Referenced Schema)
_SwaggerItemsObject
  = prism (\x -> SwaggerItemsObject x) $ \x -> case x of
      SwaggerItemsPrimitive c p -> Left (SwaggerItemsPrimitive c p)
      SwaggerItemsObject o      -> Right o
      SwaggerItemsArray a       -> Left (SwaggerItemsArray a)

_SwaggerItemsPrimitive :: forall t p f. (Profunctor p, Bifunctor p, Functor f) => Optic' p f (SwaggerItems t) (Maybe (CollectionFormat t), ParamSchema t)
_SwaggerItemsPrimitive = unto (\(c, p) -> SwaggerItemsPrimitive c p)

-- ** 'ParamSchema' lenses
makeLensesWith swaggerFieldRules ''ParamSchema
-- ** 'Xml' lenses
makeFields ''Xml
-- ** 'Responses' lenses
makeLensesWith swaggerFieldRules ''Responses
-- ** 'Response' lenses
makeFields ''Response
-- ** 'SecurityScheme' lenses
makeLensesWith swaggerFieldRules ''SecurityScheme
-- ** 'SecuritySchemeType' prisms
makePrisms ''SecuritySchemeType
-- ** 'ApiKeyParams' lenses
makeFields ''ApiKeyParams
-- ** 'OAuth2Params' lenses
makeFields ''OAuth2Params
-- ** 'ExternalDocs' lenses
makeFields ''ExternalDocs

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

-- HasType instances
instance HasType Header (SwaggerType Header) where type_ = paramSchema.type_
instance HasType Schema (SwaggerType Schema) where type_ = paramSchema.type_
instance HasType NamedSchema (SwaggerType Schema) where type_ = schema.paramSchema.type_
instance HasType ParamOtherSchema (SwaggerType ParamOtherSchema) where type_ = paramSchema.type_

-- HasDefault instances
instance HasDefault Header (Maybe Value) where default_ = paramSchema.default_
instance HasDefault Schema (Maybe Value) where default_ = paramSchema.default_
instance HasDefault ParamOtherSchema (Maybe Value) where default_ = paramSchema.default_

-- OVERLAPPABLE instances

instance {-# OVERLAPPABLE #-} HasParamSchema s (ParamSchema t)
  => HasFormat s (Maybe Format) where
  format = paramSchema.format

instance {-# OVERLAPPABLE #-} HasParamSchema s (ParamSchema t)
  => HasItems s (Maybe (SwaggerItems t)) where
  items = paramSchema.items

instance {-# OVERLAPPABLE #-} HasParamSchema s (ParamSchema t)
  => HasMaximum s (Maybe Scientific) where
  maximum_ = paramSchema.maximum_

instance {-# OVERLAPPABLE #-} HasParamSchema s (ParamSchema t)
  => HasExclusiveMaximum s (Maybe Bool) where
  exclusiveMaximum = paramSchema.exclusiveMaximum

instance {-# OVERLAPPABLE #-} HasParamSchema s (ParamSchema t)
  => HasMinimum s (Maybe Scientific) where
  minimum_ = paramSchema.minimum_

instance {-# OVERLAPPABLE #-} HasParamSchema s (ParamSchema t)
  => HasExclusiveMinimum s (Maybe Bool) where
  exclusiveMinimum = paramSchema.exclusiveMinimum

instance {-# OVERLAPPABLE #-} HasParamSchema s (ParamSchema t)
  => HasMaxLength s (Maybe Integer) where
  maxLength = paramSchema.maxLength

instance {-# OVERLAPPABLE #-} HasParamSchema s (ParamSchema t)
  => HasMinLength s (Maybe Integer) where
  minLength = paramSchema.minLength

instance {-# OVERLAPPABLE #-} HasParamSchema s (ParamSchema t)
  => HasPattern s (Maybe Text) where
  pattern = paramSchema.pattern

instance {-# OVERLAPPABLE #-} HasParamSchema s (ParamSchema t)
  => HasMaxItems s (Maybe Integer) where
  maxItems = paramSchema.maxItems

instance {-# OVERLAPPABLE #-} HasParamSchema s (ParamSchema t)
  => HasMinItems s (Maybe Integer) where
  minItems = paramSchema.minItems

instance {-# OVERLAPPABLE #-} HasParamSchema s (ParamSchema t)
  => HasUniqueItems s (Maybe Bool) where
  uniqueItems = paramSchema.uniqueItems

instance {-# OVERLAPPABLE #-} HasParamSchema s (ParamSchema t)
  => HasEnum s (Maybe [Value]) where
  enum_ = paramSchema.enum_

instance {-# OVERLAPPABLE #-} HasParamSchema s (ParamSchema t)
  => HasMultipleOf s (Maybe Scientific) where
  multipleOf = paramSchema.multipleOf
