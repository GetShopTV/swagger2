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
    -- | Specifies how to encode constructors of a sum datatype.
  , sumEncoding :: Aeson.SumEncoding
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
-- , 'sumEncoding'            = 'Aeson.defaultTaggedObject'
-- }
-- @
defaultSchemaOptions :: SchemaOptions
defaultSchemaOptions = SchemaOptions
  { fieldLabelModifier = id
  , constructorTagModifier = id
  , datatypeNameModifier = id
  , allNullaryToStringTag = True
  , unwrapUnaryRecords = False
  , sumEncoding = Aeson.defaultTaggedObject
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
-- * 'Aeson.tagSingleConstructors'
--
-- The rest is defined as in 'defaultSchemaOptions'.
--
-- @since 2.2.1
--
fromAesonOptions :: Aeson.Options -> SchemaOptions
fromAesonOptions opts = defaultSchemaOptions
  { fieldLabelModifier     = Aeson.fieldLabelModifier     opts
  , constructorTagModifier = Aeson.constructorTagModifier opts
  , allNullaryToStringTag  = Aeson.allNullaryToStringTag  opts
  , unwrapUnaryRecords     = Aeson.unwrapUnaryRecords     opts
  , sumEncoding            = Aeson.sumEncoding            opts
  }
