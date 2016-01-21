-- |
-- Module:      Data.Swagger.Schema
-- Copyright:   (c) 2015 GetShopTV
-- License:     BSD3
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Types and functions for working with Swagger schema.
module Data.Swagger.Schema (
  -- * Encoding
  ToSchema(..),
  Definitions,
  declareSchema,
  declareSchemaRef,
  toSchema,
  toSchemaRef,
  schemaName,
  toInlinedSchema,

  -- * Generic schema encoding
  genericDeclareNamedSchema,
  genericDeclareSchema,
  genericToNamedSchemaBoundedIntegral,
  toSchemaBoundedIntegral,
  paramSchemaToNamedSchema,
  paramSchemaToSchema,

  -- * Inlining @'Schema'@s
  inlineNonRecursiveSchemas,
  inlineAllSchemas,
  inlineSchemas,
  inlineSchemasWhen,

  -- * Generic encoding configuration
  SchemaOptions(..),
  defaultSchemaOptions,
) where

import Data.Swagger.Internal.Schema
import Data.Swagger.SchemaOptions
