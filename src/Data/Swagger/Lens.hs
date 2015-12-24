{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Swagger.Lens where

import Control.Lens
import Data.Aeson (Value)
import Data.Scientific (Scientific)
import Data.Swagger.Internal
import Data.Text (Text)

-- =======================================================================
-- * TH derived lenses

-- ** 'Swagger' lenses
makeLenses ''Swagger
-- ** 'Host' lenses
makeLenses ''Host
-- ** 'Info' lenses
makeLenses ''Info
-- ** 'Contact' lenses
makeLenses ''Contact
-- ** 'License' lenses
makeLenses ''License
-- ** 'Paths' lenses
makeLenses ''Paths
-- ** 'PathItem' lenses
makeLenses ''PathItem
-- ** 'Tag' lenses
makeLenses ''Tag
-- ** 'Operation' lenses
makeLenses ''Operation
-- ** 'Param' lenses
makeLenses ''Param
-- ** 'ParamAnySchema' prisms
makePrisms ''ParamAnySchema
-- ** 'ParamOtherSchema' lenses
makeLenses ''ParamOtherSchema
-- ** 'Header' lenses
makeLenses ''Header
-- ** 'Schema' lenses
makeLenses ''Schema
-- ** 'SwaggerItems' prisms
makePrisms ''SwaggerItems
-- ** 'ParamSchema' lenses
makeLenses ''ParamSchema
-- ** 'Xml' lenses
makeLenses ''Xml
-- ** 'Responses' lenses
makeLenses ''Responses
-- ** 'Response' lenses
makeLenses ''Response
-- ** 'SecurityScheme' lenses
makeLenses ''SecurityScheme
-- ** 'SecuritySchemeType' prisms
makePrisms ''SecuritySchemeType
-- ** 'ApiKeyParams' lenses
makeLenses ''ApiKeyParams
-- ** 'OAuth2Params' lenses
makeLenses ''OAuth2Params
-- ** 'ExternalDocs' lenses
makeLenses ''ExternalDocs

-- =======================================================================
-- * Helper classy lenses

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

class HasParamSchema s t | s -> t where
  parameterSchema :: Lens' s (ParamSchema t)

instance HasParamSchema Schema Schema where parameterSchema = schemaParamSchema
instance HasParamSchema ParamOtherSchema ParamOtherSchema where parameterSchema = paramOtherSchemaParamSchema
instance HasParamSchema Header Header where parameterSchema = headerParamSchema
instance HasParamSchema (ParamSchema t) t where parameterSchema = id

schemaType :: HasParamSchema s t => Lens' s (SwaggerType t)
schemaType = parameterSchema.paramSchemaType

schemaFormat :: HasParamSchema s t => Lens' s (Maybe Format)
schemaFormat = parameterSchema.paramSchemaFormat

schemaItems :: HasParamSchema s t => Lens' s (Maybe (SwaggerItems t))
schemaItems = parameterSchema.paramSchemaItems

schemaDefault :: HasParamSchema s t => Lens' s (Maybe Value)
schemaDefault = parameterSchema.paramSchemaDefault

schemaMaximum :: HasParamSchema s t => Lens' s (Maybe Scientific)
schemaMaximum = parameterSchema.paramSchemaMaximum

schemaExclusiveMaximum :: HasParamSchema s t => Lens' s (Maybe Bool)
schemaExclusiveMaximum = parameterSchema.paramSchemaExclusiveMaximum

schemaMinimum :: HasParamSchema s t => Lens' s (Maybe Scientific)
schemaMinimum = parameterSchema.paramSchemaMinimum

schemaExclusiveMinimum :: HasParamSchema s t => Lens' s (Maybe Bool)
schemaExclusiveMinimum = parameterSchema.paramSchemaExclusiveMinimum

schemaMaxLength :: HasParamSchema s t => Lens' s (Maybe Integer)
schemaMaxLength = parameterSchema.paramSchemaMaxLength

schemaMinLength :: HasParamSchema s t => Lens' s (Maybe Integer)
schemaMinLength = parameterSchema.paramSchemaMinLength

schemaPattern :: HasParamSchema s t => Lens' s (Maybe Text)
schemaPattern = parameterSchema.paramSchemaPattern

schemaMaxItems :: HasParamSchema s t => Lens' s (Maybe Integer)
schemaMaxItems = parameterSchema.paramSchemaMaxItems

schemaMinItems :: HasParamSchema s t => Lens' s (Maybe Integer)
schemaMinItems = parameterSchema.paramSchemaMinItems

schemaUniqueItems :: HasParamSchema s t => Lens' s (Maybe Bool)
schemaUniqueItems = parameterSchema.paramSchemaUniqueItems

schemaEnum :: HasParamSchema s t => Lens' s (Maybe [Value])
schemaEnum = parameterSchema.paramSchemaEnum

schemaMultipleOf :: HasParamSchema s t => Lens' s (Maybe Scientific)
schemaMultipleOf = parameterSchema.paramSchemaMultipleOf

