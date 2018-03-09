{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:      Data.Swagger.SchemaOptions
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Generic deriving options for @'ToParamSchema'@ and @'ToSchema'@.
module Data.Swagger.SchemaOptions where

import qualified Data.Aeson.Types as Aeson

-- | Options that specify how to encode your type to Swagger schema.
data SchemaOptions = SchemaOptions
  { -- | Function applied to field labels. Handy for removing common record prefixes for example.
    fieldLabelModifier :: String -> String
    -- | Function applied to constructor tags which could be handy for lower-casing them for example.
  , constructorTagModifier :: String -> String
    -- | Function applied to datatype name.
  , datatypeNameModifier :: String -> String
    -- | If @'True'@ the constructors of a datatype, with all nullary constructors,
    -- will be encoded to a string enumeration schema with the constructor tags as possible values.
  , allNullaryToStringTag :: Bool
    -- | Hide the field name when a record constructor has only one field, like a newtype.
  , unwrapUnaryRecords :: Bool
  }

-- | Default encoding @'SchemaOptions'@.
--
-- @
-- 'SchemaOptions'
-- { 'fieldLabelModifier'     = id
-- , 'constructorTagModifier' = id
-- , 'datatypeNameModifier'   = id
-- , 'allNullaryToStringTag'  = True
-- , 'unwrapUnaryRecords'     = False
-- }
-- @
defaultSchemaOptions :: SchemaOptions
defaultSchemaOptions = SchemaOptions
  { fieldLabelModifier = id
  , constructorTagModifier = id
  , datatypeNameModifier = id
  , allNullaryToStringTag = True
  , unwrapUnaryRecords = False
  }

-- | Convert 'Aeson.Options' to 'SchemaOptions'.
--
-- Specifically the following fields get copied:
--
-- * 'fieldLabelModifier'
-- * 'constructorTagModifier'
-- * 'allNullaryToStringTag'
-- * 'unwrapUnaryRecords'
--
-- Note that these fields have no effect on `SchemaOptions`:
--
-- * 'Aeson.omitNothingFields'
-- * 'Aeson.sumEncoding'
-- * 'Aeson.tagSingleConstructors'
--
-- The rest is defined as in 'defaultSchemaOptions'.
fromAesonOptions :: Aeson.Options -> SchemaOptions
fromAesonOptions opts = defaultSchemaOptions
  { fieldLabelModifier     = Aeson.fieldLabelModifier     opts
  , constructorTagModifier = Aeson.constructorTagModifier opts
  , allNullaryToStringTag  = Aeson.allNullaryToStringTag  opts
  , unwrapUnaryRecords     = Aeson.unwrapUnaryRecords     opts
  }
