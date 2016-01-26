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

  -- * Schema templates
  passwordSchema,
  binarySchema,
  byteSchema,

  -- * Sketching @'Schema'@s using @'ToJSON'@
  sketchSchema,
  sketchStrictSchema,

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
