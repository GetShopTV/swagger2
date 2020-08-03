-- |
-- Module:      Data.Swagger.Schema
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
  genericDeclareNamedSchemaNewtype,
  genericNameSchema,

  -- ** 'Bounded' 'Integral'
  genericToNamedSchemaBoundedIntegral,
  toSchemaBoundedIntegral,

  -- ** 'Bounded' 'Enum' key mappings
  declareSchemaBoundedEnumKeyMapping,
  toSchemaBoundedEnumKeyMapping,

  -- ** Reusing 'ToParamSchema'
  paramSchemaToNamedSchema,
  paramSchemaToSchema,

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
  fromAesonOptions,
) where

import Data.Swagger.Internal.Schema
import Data.Swagger.SchemaOptions
