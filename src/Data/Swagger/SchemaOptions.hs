module Data.Swagger.SchemaOptions where

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
    -- | If @'True'@ direct subschemas will be referenced if possible (rather than inlined).
    -- Note that this option does not influence nested schemas, e.g. for these types
    --
    -- @
    -- data Object = Object String deriving Generic
    -- instance ToSchema Object
    --
    -- newtype Objects = Objects [Object] deriving Generic
    -- instance ToSchema Objects where
    --    toNamedSchema = genericToNamedSchema defaultSchemaOptions
    --      { useReferences = False }
    -- @
    --
    -- Schema for @Objects@ __will not__ inline @Object@ schema because
    -- it is nested in a @[]@ schema.
  , useReferences :: Bool
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
-- , 'useReferences'          = True
-- , 'unwrapUnaryRecords'     = False
-- }
-- @
defaultSchemaOptions :: SchemaOptions
defaultSchemaOptions = SchemaOptions
  { fieldLabelModifier = id
  , constructorTagModifier = id
  , datatypeNameModifier = id
  , allNullaryToStringTag = True
  , useReferences = True
  , unwrapUnaryRecords = False
  }

