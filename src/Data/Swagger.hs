-- |
-- Swagger™ is a project used to describe and document RESTful APIs.
--
-- The Swagger specification defines a set of files required to describe such an API.
-- These files can then be used by the Swagger-UI project to display the API
-- and Swagger-Codegen to generate clients in various languages.
-- Additional utilities can also take advantage of the resulting files, such as testing tools.
module Data.Swagger (
  -- * Swagger specification
  Swagger(..),
  SwaggerHost(..),
  SwaggerScheme(..),

  -- * Info types
  SwaggerInfo(..),
  SwaggerContact(..),
  SwaggerLicense(..),

  -- * Paths
  SwaggerPaths(..),
  SwaggerPathItem(..),

  -- * Operations
  SwaggerTag(..),
  SwaggerOperation(..),

  -- * Types and formats
  SwaggerItemsType(..),
  SwaggerSchemaType(..),
  SwaggerFormat(..),
  SwaggerCollectionFormat(..),
  SwaggerItemsCollectionFormat(..),

  -- * Parameters
  SwaggerParameter(..),
  SwaggerParameterSchema(..),
  SwaggerParameterOtherSchema(..),
  SwaggerParameterLocation(..),
  SwaggerItems(..),
  SwaggerHeader(..),
  SwaggerExample(..),

  -- * Schema
  SwaggerSchema(..),
  SwaggerSchemaItems(..),
  SwaggerSchemaCommon(..),
  SwaggerXml(..),

  -- * Responses
  SwaggerResponses(..),
  SwaggerResponse(..),

  -- * Security
  SwaggerSecurityScheme(..),
  SwaggerSecuritySchemeType(..),
  SwaggerSecurityRequirement(..),

  -- ** API key
  SwaggerApiKeyParams(..),
  SwaggerApiKeyLocation(..),

  -- ** OAuth2
  SwaggerOAuth2Params(..),
  SwaggerOAuth2Flow(..),
  AuthorizationURL(..),
  TokenURL(..),

  -- * External documentation
  SwaggerExternalDocs(..),

  -- * Miscellaneous
  SwaggerMimeList(..),
  URL(..),
) where

import Data.Swagger.Internal

