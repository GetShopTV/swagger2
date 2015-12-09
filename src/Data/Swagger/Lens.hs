{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Swagger.Lens where

import Control.Lens
import Data.Aeson (Value)
import Data.Scientific (Scientific)
import Data.Swagger.Internal
import Data.Text (Text)

-- =======================================================================
-- TH derived lenses
-- =======================================================================

makeLenses ''Swagger
makeLenses ''Host
makeLenses ''Info
makeLenses ''Contact
makeLenses ''License
makeLenses ''Paths
makeLenses ''PathItem
makeLenses ''Tag
makeLenses ''Operation
makeLenses ''Param
makePrisms ''ParamSchema
makeLenses ''ParamOtherSchema
makeLenses ''Items
makeLenses ''Header
makeLenses ''Schema
makePrisms ''SchemaItems
makeLenses ''SchemaCommon
makeLenses ''Xml
makeLenses ''Responses
makeLenses ''Response
makeLenses ''SecurityScheme
makePrisms ''SecuritySchemeType
makeLenses ''ApiKeyParams
makeLenses ''OAuth2Params
makeLenses ''ExternalDocs

-- =======================================================================
-- Helper classy lenses
-- =======================================================================

class HasDescription s d | s -> d where
  description :: Lens' s d

instance HasDescription Response       Text where description = responseDescription
instance HasDescription Info           (Maybe Text) where description = infoDescription
instance HasDescription Tag            (Maybe Text) where description = tagDescription
instance HasDescription Operation      (Maybe Text) where description = operationDescription
instance HasDescription Param          (Maybe Text) where description = paramDescription
instance HasDescription Header         (Maybe Text) where description = headerDescription
instance HasDescription Schema         (Maybe Text) where description = schemaDescription
instance HasDescription SecurityScheme (Maybe Text) where description = securitySchemeDescription
instance HasDescription ExternalDocs   (Maybe Text) where description = externalDocsDescription

class HasSchemaCommon s t i | s -> t i where
  schemaCommon :: Lens' s (SchemaCommon t i)

instance HasSchemaCommon Schema Schema SchemaItems where schemaCommon = schemaSchemaCommon
instance HasSchemaCommon ParamOtherSchema Param Items where schemaCommon = paramOtherSchemaCommon
instance HasSchemaCommon Items Items Items where schemaCommon = itemsCommon
instance HasSchemaCommon Header Items Items where schemaCommon = headerCommon
instance HasSchemaCommon (SchemaCommon t i) t i where schemaCommon = id

schemaType :: HasSchemaCommon s t i => Lens' s (SwaggerType t)
schemaType = schemaCommon.schemaCommonType

schemaFormat :: HasSchemaCommon s t i => Lens' s (Maybe Format)
schemaFormat = schemaCommon.schemaCommonFormat

schemaItems :: HasSchemaCommon s t i => Lens' s (Maybe i)
schemaItems = schemaCommon.schemaCommonItems

schemaDefault :: HasSchemaCommon s t i => Lens' s (Maybe Value)
schemaDefault = schemaCommon.schemaCommonDefault

schemaMaximum :: HasSchemaCommon s t i => Lens' s (Maybe Scientific)
schemaMaximum = schemaCommon.schemaCommonMaximum

schemaExclusiveMaximum :: HasSchemaCommon s t i => Lens' s (Maybe Bool)
schemaExclusiveMaximum = schemaCommon.schemaCommonExclusiveMaximum

schemaMinimum :: HasSchemaCommon s t i => Lens' s (Maybe Scientific)
schemaMinimum = schemaCommon.schemaCommonMinimum

schemaExclusiveMinimum :: HasSchemaCommon s t i => Lens' s (Maybe Bool)
schemaExclusiveMinimum = schemaCommon.schemaCommonExclusiveMinimum

schemaMaxLength :: HasSchemaCommon s t i => Lens' s (Maybe Integer)
schemaMaxLength = schemaCommon.schemaCommonMaxLength

schemaMinLength :: HasSchemaCommon s t i => Lens' s (Maybe Integer)
schemaMinLength = schemaCommon.schemaCommonMinLength

schemaPattern :: HasSchemaCommon s t i => Lens' s (Maybe Text)
schemaPattern = schemaCommon.schemaCommonPattern

schemaMaxItems :: HasSchemaCommon s t i => Lens' s (Maybe Integer)
schemaMaxItems = schemaCommon.schemaCommonMaxItems

schemaMinItems :: HasSchemaCommon s t i => Lens' s (Maybe Integer)
schemaMinItems = schemaCommon.schemaCommonMinItems

schemaUniqueItems :: HasSchemaCommon s t i => Lens' s (Maybe Bool)
schemaUniqueItems = schemaCommon.schemaCommonUniqueItems

schemaEnum :: HasSchemaCommon s t i => Lens' s (Maybe [Value])
schemaEnum = schemaCommon.schemaCommonEnum

schemaMultipleOf :: HasSchemaCommon s t i => Lens' s (Maybe Scientific)
schemaMultipleOf = schemaCommon.schemaCommonMultipleOf

