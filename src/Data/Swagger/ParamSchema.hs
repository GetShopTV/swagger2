-- |
-- Module:      Data.Swagger.ParamSchema
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Types and functions for working with Swagger parameter schema.
module Data.Swagger.ParamSchema (
  -- * Encoding
  ToParamSchema(..),

  -- * Generic schema encoding
  genericToParamSchema,
  toParamSchemaBoundedIntegral,

  -- * Schema templates
  passwordSchema,
  binarySchema,
  byteSchema,

  -- * Generic encoding configuration
  SchemaOptions(..),
  defaultSchemaOptions,
) where

import Data.Swagger.Internal.ParamSchema
import Data.Swagger.SchemaOptions
