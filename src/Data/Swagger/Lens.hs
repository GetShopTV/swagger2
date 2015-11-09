{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Swagger.Lens where

import Control.Lens
import Control.Lens.TH
import Data.Aeson (Value)
import Data.Swagger.Internal
import Data.Text (Text)

-- =======================================================================
-- TH derived lenses
-- =======================================================================

makeLenses ''Swagger
makeLenses ''SwaggerHost
makeLenses ''SwaggerInfo
makeLenses ''SwaggerContact
makeLenses ''SwaggerLicense
makeLenses ''SwaggerPaths
makeLenses ''SwaggerPathItem
makeLenses ''SwaggerTag
makeLenses ''SwaggerOperation
makeLenses ''SwaggerParameter
makePrisms ''SwaggerParameterSchema
makeLenses ''SwaggerParameterOtherSchema
makeLenses ''SwaggerItems
makeLenses ''SwaggerHeader
makeLenses ''SwaggerSchema
makePrisms ''SwaggerSchemaItems
makeLenses ''SwaggerSchemaCommon
makeLenses ''SwaggerXml
makeLenses ''SwaggerResponses
makeLenses ''SwaggerResponse
makeLenses ''SwaggerSecurityScheme
makePrisms ''SwaggerSecuritySchemeType
makeLenses ''SwaggerApiKeyParams
makeLenses ''SwaggerOAuth2Params
makeLenses ''SwaggerExternalDocs

-- =======================================================================
-- Helper classy lenses
-- =======================================================================

class HasDescription s d | s -> d where
  description :: Lens' s d

instance HasDescription SwaggerResponse       Text where description = swaggerResponseDescription
instance HasDescription SwaggerInfo           (Maybe Text) where description = swaggerInfoDescription
instance HasDescription SwaggerTag            (Maybe Text) where description = swaggerTagDescription
instance HasDescription SwaggerOperation      (Maybe Text) where description = swaggerOperationDescription
instance HasDescription SwaggerParameter      (Maybe Text) where description = swaggerParameterDescription
instance HasDescription SwaggerHeader         (Maybe Text) where description = swaggerHeaderDescription
instance HasDescription SwaggerSchema         (Maybe Text) where description = swaggerSchemaDescription
instance HasDescription SwaggerSecurityScheme (Maybe Text) where description = swaggerSecuritySchemeDescription
instance HasDescription SwaggerExternalDocs   (Maybe Text) where description = swaggerExternalDocsDescription

class HasSwaggerSchemaCommon s where
  schemaCommon :: Lens' s SwaggerSchemaCommon

instance HasSwaggerSchemaCommon SwaggerParameter where
  schemaCommon = swaggerParameterSchema.schemaCommon

instance HasSwaggerSchemaCommon SwaggerParameterSchema where
  schemaCommon = paramSchemaEither . choosing schemaCommon schemaCommon
    where
      paramSchemaEither = iso toEither fromEither
      toEither (SwaggerParameterBody  x) = Left x
      toEither (SwaggerParameterOther y) = Right y
      fromEither (Left  x) = SwaggerParameterBody x
      fromEither (Right y) = SwaggerParameterOther y

instance HasSwaggerSchemaCommon SwaggerSchema where schemaCommon = swaggerSchemaCommon
instance HasSwaggerSchemaCommon SwaggerParameterOtherSchema where schemaCommon = swaggerParameterOtherSchemaCommon
instance HasSwaggerSchemaCommon SwaggerItems where schemaCommon = swaggerItemsCommon
instance HasSwaggerSchemaCommon SwaggerHeader where schemaCommon = swaggerHeaderCommon
instance HasSwaggerSchemaCommon SwaggerSchemaCommon where schemaCommon = id

schemaDefault :: HasSwaggerSchemaCommon s => Lens' s (Maybe Value)
schemaDefault = schemaCommon.swaggerSchemaDefault

schemaMaximum :: HasSwaggerSchemaCommon s => Lens' s (Maybe Integer)
schemaMaximum = schemaCommon.swaggerSchemaMaximum

schemaExclusiveMaximum :: HasSwaggerSchemaCommon s => Lens' s (Maybe Bool)
schemaExclusiveMaximum = schemaCommon.swaggerSchemaExclusiveMaximum

schemaMinimum :: HasSwaggerSchemaCommon s => Lens' s (Maybe Integer)
schemaMinimum = schemaCommon.swaggerSchemaMinimum

schemaExclusiveMinimum :: HasSwaggerSchemaCommon s => Lens' s (Maybe Bool)
schemaExclusiveMinimum = schemaCommon.swaggerSchemaExclusiveMinimum

schemaMaxLength :: HasSwaggerSchemaCommon s => Lens' s (Maybe Integer)
schemaMaxLength = schemaCommon.swaggerSchemaMaxLength

schemaMinLength :: HasSwaggerSchemaCommon s => Lens' s (Maybe Integer)
schemaMinLength = schemaCommon.swaggerSchemaMinLength

schemaPattern :: HasSwaggerSchemaCommon s => Lens' s (Maybe Text)
schemaPattern = schemaCommon.swaggerSchemaPattern

schemaMaxItems :: HasSwaggerSchemaCommon s => Lens' s (Maybe Integer)
schemaMaxItems = schemaCommon.swaggerSchemaMaxItems

schemaMinItems :: HasSwaggerSchemaCommon s => Lens' s (Maybe Integer)
schemaMinItems = schemaCommon.swaggerSchemaMinItems

schemaUniqueItems :: HasSwaggerSchemaCommon s => Lens' s (Maybe Bool)
schemaUniqueItems = schemaCommon.swaggerSchemaUniqueItems

schemaEnum :: HasSwaggerSchemaCommon s => Lens' s (Maybe [Value])
schemaEnum = schemaCommon.swaggerSchemaEnum

schemaMultipleOf :: HasSwaggerSchemaCommon s => Lens' s (Maybe Integer)
schemaMultipleOf = schemaCommon.swaggerSchemaMultipleOf

