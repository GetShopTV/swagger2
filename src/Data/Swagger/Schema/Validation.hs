-- |
-- Module:      Data.Swagger.Schema.Validation
-- Copyright:   (c) 2015 GetShopTV
-- License:     BSD3
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Validate JSON values with Swagger Schema.
module Data.Swagger.Schema.Validation (
  validateToJSON,
  validateToJSONWithPatternChecker,
  ValidationError,
) where

import Data.Swagger.Internal.Schema.Validation
