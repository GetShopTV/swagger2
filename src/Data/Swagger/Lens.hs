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

