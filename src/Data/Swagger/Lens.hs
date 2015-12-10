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
makePrisms ''ParamAnySchema
makeLenses ''ParamOtherSchema
makeLenses ''Items
makeLenses ''Header
makeLenses ''Schema
makePrisms ''SchemaItems
makeLenses ''ParamSchema
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

class HasParamSchema s t i | s -> t i where
  parameterSchema :: Lens' s (ParamSchema t i)

instance HasParamSchema Schema Schema SchemaItems where parameterSchema = schemaParamSchema
instance HasParamSchema ParamOtherSchema ParamOtherSchema Items where parameterSchema = paramOtherSchemaParamSchema
instance HasParamSchema Items Items Items where parameterSchema = itemsParamSchema
instance HasParamSchema Header Header Items where parameterSchema = headerParamSchema
instance HasParamSchema (ParamSchema t i) t i where parameterSchema = id

schemaType :: HasParamSchema s t i => Lens' s (SwaggerType t)
schemaType = parameterSchema.paramSchemaType

schemaFormat :: HasParamSchema s t i => Lens' s (Maybe Format)
schemaFormat = parameterSchema.paramSchemaFormat

schemaItems :: HasParamSchema s t i => Lens' s (Maybe i)
schemaItems = parameterSchema.paramSchemaItems

schemaDefault :: HasParamSchema s t i => Lens' s (Maybe Value)
schemaDefault = parameterSchema.paramSchemaDefault

schemaMaximum :: HasParamSchema s t i => Lens' s (Maybe Scientific)
schemaMaximum = parameterSchema.paramSchemaMaximum

schemaExclusiveMaximum :: HasParamSchema s t i => Lens' s (Maybe Bool)
schemaExclusiveMaximum = parameterSchema.paramSchemaExclusiveMaximum

schemaMinimum :: HasParamSchema s t i => Lens' s (Maybe Scientific)
schemaMinimum = parameterSchema.paramSchemaMinimum

schemaExclusiveMinimum :: HasParamSchema s t i => Lens' s (Maybe Bool)
schemaExclusiveMinimum = parameterSchema.paramSchemaExclusiveMinimum

schemaMaxLength :: HasParamSchema s t i => Lens' s (Maybe Integer)
schemaMaxLength = parameterSchema.paramSchemaMaxLength

schemaMinLength :: HasParamSchema s t i => Lens' s (Maybe Integer)
schemaMinLength = parameterSchema.paramSchemaMinLength

schemaPattern :: HasParamSchema s t i => Lens' s (Maybe Text)
schemaPattern = parameterSchema.paramSchemaPattern

schemaMaxItems :: HasParamSchema s t i => Lens' s (Maybe Integer)
schemaMaxItems = parameterSchema.paramSchemaMaxItems

schemaMinItems :: HasParamSchema s t i => Lens' s (Maybe Integer)
schemaMinItems = parameterSchema.paramSchemaMinItems

schemaUniqueItems :: HasParamSchema s t i => Lens' s (Maybe Bool)
schemaUniqueItems = parameterSchema.paramSchemaUniqueItems

schemaEnum :: HasParamSchema s t i => Lens' s (Maybe [Value])
schemaEnum = parameterSchema.paramSchemaEnum

schemaMultipleOf :: HasParamSchema s t i => Lens' s (Maybe Scientific)
schemaMultipleOf = parameterSchema.paramSchemaMultipleOf

