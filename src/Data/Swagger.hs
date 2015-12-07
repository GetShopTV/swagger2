-- |
-- Swagger™ is a project used to describe and document RESTful APIs.
--
-- The Swagger specification defines a set of files required to describe such an API.
-- These files can then be used by the Swagger-UI project to display the API
-- and Swagger-Codegen to generate clients in various languages.
-- Additional utilities can also take advantage of the resulting files, such as testing tools.
module Data.Swagger (
  module Data.Swagger.Schema,

  -- * Swagger specification
  Swagger(..),
  Host(..),
  Scheme(..),

  -- * Info types
  Info(..),
  Contact(..),
  License(..),

  -- * Paths
  Paths(..),
  PathItem(..),

  -- * Operations
  Tag(..),
  Operation(..),

  -- * Types and formats
  ParamType(..),
  ItemsType(..),
  SchemaType(..),
  Format,
  CollectionFormat(..),
  ItemsCollectionFormat(..),

  -- * Parameters
  Param(..),
  ParamSchema(..),
  ParamOtherSchema(..),
  ParamLocation(..),
  ParamName,
  Items(..),
  Header(..),
  Example(..),

  -- * Schema
  Schema(..),
  SchemaItems(..),
  SchemaCommon(..),
  Xml(..),

  -- * Responses
  Responses(..),
  Response(..),

  -- * Security
  SecurityScheme(..),
  SecuritySchemeType(..),
  SecurityRequirement(..),

  -- ** API key
  ApiKeyParams(..),
  ApiKeyLocation(..),

  -- ** OAuth2
  OAuth2Params(..),
  OAuth2Flow(..),
  AuthorizationURL,
  TokenURL,

  -- * External documentation
  ExternalDocs(..),

  -- * References
  Reference(..),
  Referenced(..),

  -- * Miscellaneous
  MimeList(..),
  URL(..),
) where

import Data.Swagger.Schema

import Data.Swagger.Internal

