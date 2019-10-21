{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module:      Data.Swagger.Optics
-- Maintainer:  Andrzej Rybczak <andrzej@rybczak.net>
-- Stability:   experimental
--
-- Lenses and prisms for the <https://hackage.haskell.org/package/optics optics>
-- library.
--
-- >>> import Data.Aeson
-- >>> import Optics.Core
-- >>> :set -XOverloadedLabels
--
-- Example from the "Data.Swagger" module using @optics@:
--
-- >>> :{
-- encode $ (mempty :: Swagger)
--   & #definitions .~ [ ("User", mempty & #type ?~ SwaggerString) ]
--   & #paths .~
--     [ ("/user", mempty & #get ?~ (mempty
--         & #produces ?~ MimeList ["application/json"]
--         & at 200 ?~ ("OK" & #_Inline % #schema ?~ Ref (Reference "User"))
--         & at 404 ?~ "User info not found")) ]
-- :}
-- "{\"swagger\":\"2.0\",\"info\":{\"version\":\"\",\"title\":\"\"},\"paths\":{\"/user\":{\"get\":{\"produces\":[\"application/json\"],\"responses\":{\"404\":{\"description\":\"User info not found\"},\"200\":{\"schema\":{\"$ref\":\"#/definitions/User\"},\"description\":\"OK\"}}}}},\"definitions\":{\"User\":{\"type\":\"string\"}}}"
--
-- For convenience optics are defined as /labels/. It means that field accessor
-- names can be overloaded for different types. One such common field is
-- @#description@. Many components of a Swagger specification can have
-- descriptions, and you can use the same name for them:
--
-- >>> encode $ (mempty :: Response) & #description .~ "No content"
-- "{\"description\":\"No content\"}"
-- >>> :{
-- encode $ (mempty :: Schema)
--   & #type        ?~ SwaggerBoolean
--   & #description ?~ "To be or not to be"
-- :}
-- "{\"description\":\"To be or not to be\",\"type\":\"boolean\"}"
--
-- @'ParamSchema'@ is basically the /base schema specification/ and many types
-- contain it. So for convenience, all @'ParamSchema'@ fields are transitively
-- made fields of the type that has it. For example, you can use @#type@ to
-- access @'SwaggerType'@ of @'Header'@ schema without having to use
-- @#paramSchema@:
--
-- >>> encode $ (mempty :: Header) & #type ?~ SwaggerNumber
-- "{\"type\":\"number\"}"
--
-- Additionally, to simplify working with @'Response'@, both @'Operation'@ and
-- @'Responses'@ have direct access to it via @'Optics.Core.At.at'@. Example:
--
-- >>> :{
-- encode $ (mempty :: Operation)
--   & at 404 ?~ "Not found"
-- :}
-- "{\"responses\":{\"404\":{\"description\":\"Not found\"}}}"
--
module Data.Swagger.Optics () where

import Data.Aeson (Value)
import Data.Scientific (Scientific)
import Data.Swagger.Internal
import Data.Text (Text)
import Optics.Core
import Optics.TH

-- Lenses

makeFieldLabels ''Swagger
makeFieldLabels ''Host
makeFieldLabels ''Info
makeFieldLabels ''Contact
makeFieldLabels ''License
makeFieldLabels ''PathItem
makeFieldLabels ''Tag
makeFieldLabels ''Operation
makeFieldLabels ''Param
makeFieldLabels ''ParamOtherSchema
makeFieldLabels ''Header
makeFieldLabels ''Schema
makeFieldLabels ''NamedSchema
makeFieldLabels ''ParamSchema
makeFieldLabels ''Xml
makeFieldLabels ''Responses
makeFieldLabels ''Response
makeFieldLabels ''SecurityScheme
makeFieldLabels ''ApiKeyParams
makeFieldLabels ''OAuth2Params
makeFieldLabels ''ExternalDocs

-- Prisms

makePrismLabels ''ParamAnySchema
makePrismLabels ''SecuritySchemeType
makePrismLabels ''Referenced

-- SwaggerItems prisms

instance
  ( a ~ [Referenced Schema]
  , b ~ [Referenced Schema]
  ) => LabelOptic "_SwaggerItemsArray"
         A_Review
         (SwaggerItems 'SwaggerKindSchema)
         (SwaggerItems 'SwaggerKindSchema)
         a
         b where
  labelOptic = unto (\x -> SwaggerItemsArray x)
  {-# INLINE labelOptic #-}

instance
  ( a ~ Referenced Schema
  , b ~ Referenced Schema
  ) => LabelOptic "_SwaggerItemsObject"
         A_Review
         (SwaggerItems 'SwaggerKindSchema)
         (SwaggerItems 'SwaggerKindSchema)
         a
         b where
  labelOptic = unto (\x -> SwaggerItemsObject x)
  {-# INLINE labelOptic #-}

instance
  ( a ~ (Maybe (CollectionFormat t), ParamSchema t)
  , b ~ (Maybe (CollectionFormat t), ParamSchema t)
  ) => LabelOptic "_SwaggerItemsPrimitive"
         A_Review
         (SwaggerItems t)
         (SwaggerItems t)
         a
         b where
  labelOptic = unto (\(c, p) -> SwaggerItemsPrimitive c p)
  {-# INLINE labelOptic #-}

-- =============================================================
-- More helpful instances for easier access to schema properties

type instance Index Responses = HttpStatusCode
type instance Index Operation = HttpStatusCode

type instance IxValue Responses = Referenced Response
type instance IxValue Operation = Referenced Response

instance Ixed Responses where
  ix n = #responses % ix n
  {-# INLINE ix #-}
instance At   Responses where
  at n = #responses % at n
  {-# INLINE at #-}

instance Ixed Operation where
  ix n = #responses % ix n
  {-# INLINE ix #-}
instance At   Operation where
  at n = #responses % at n
  {-# INLINE at #-}

-- #paramSchema

instance
  ( a ~ ParamSchema 'SwaggerKindSchema
  , b ~ ParamSchema 'SwaggerKindSchema
  ) => LabelOptic "paramSchema" A_Lens NamedSchema NamedSchema a b where
  labelOptic = #schema % #paramSchema
  {-# INLINE labelOptic #-}

-- #type

instance
  ( a ~ Maybe (SwaggerType ('SwaggerKindNormal Header))
  , b ~ Maybe (SwaggerType ('SwaggerKindNormal Header))
  ) => LabelOptic "type" A_Lens Header Header a b where
  labelOptic = #paramSchema % #type
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe (SwaggerType 'SwaggerKindSchema)
  , b ~ Maybe (SwaggerType 'SwaggerKindSchema)
  ) => LabelOptic "type" A_Lens Schema Schema a b where
  labelOptic = #paramSchema % #type
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe (SwaggerType 'SwaggerKindSchema)
  , b ~ Maybe (SwaggerType 'SwaggerKindSchema)
  ) => LabelOptic "type" A_Lens NamedSchema NamedSchema a b where
  labelOptic = #paramSchema % #type
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe (SwaggerType 'SwaggerKindParamOtherSchema)
  , b ~ Maybe (SwaggerType 'SwaggerKindParamOtherSchema)
  ) => LabelOptic "type" A_Lens ParamOtherSchema ParamOtherSchema a b where
  labelOptic = #paramSchema % #type
  {-# INLINE labelOptic #-}

-- #default

instance
  ( a ~ Maybe Value, b ~ Maybe Value
  ) => LabelOptic "default" A_Lens Header Header a b where
  labelOptic = #paramSchema % #default
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Value, b ~ Maybe Value
  ) => LabelOptic "default" A_Lens Schema Schema a b where
  labelOptic = #paramSchema % #default
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Value, b ~ Maybe Value
  ) => LabelOptic "default" A_Lens NamedSchema NamedSchema a b where
  labelOptic = #paramSchema % #default
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Value, b ~ Maybe Value
  ) => LabelOptic "default" A_Lens ParamOtherSchema ParamOtherSchema a b where
  labelOptic = #paramSchema % #default
  {-# INLINE labelOptic #-}

-- #format

instance
  ( a ~ Maybe Format, b ~ Maybe Format
  ) => LabelOptic "format" A_Lens Header Header a b where
  labelOptic = #paramSchema % #format
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Format, b ~ Maybe Format
  ) => LabelOptic "format" A_Lens Schema Schema a b where
  labelOptic = #paramSchema % #format
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Format, b ~ Maybe Format
  ) => LabelOptic "format" A_Lens NamedSchema NamedSchema a b where
  labelOptic = #paramSchema % #format
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Format, b ~ Maybe Format
  ) => LabelOptic "format" A_Lens ParamOtherSchema ParamOtherSchema a b where
  labelOptic = #paramSchema % #format
  {-# INLINE labelOptic #-}

-- #items

instance
  ( a ~ Maybe (SwaggerItems ('SwaggerKindNormal Header))
  , b ~ Maybe (SwaggerItems ('SwaggerKindNormal Header))
  ) => LabelOptic "items" A_Lens Header Header a b where
  labelOptic = #paramSchema % #items
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe (SwaggerItems 'SwaggerKindSchema)
  , b ~ Maybe (SwaggerItems 'SwaggerKindSchema)
  ) => LabelOptic "items" A_Lens Schema Schema a b where
  labelOptic = #paramSchema % #items
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe (SwaggerItems 'SwaggerKindSchema)
  , b ~ Maybe (SwaggerItems 'SwaggerKindSchema)
  ) => LabelOptic "items" A_Lens NamedSchema NamedSchema a b where
  labelOptic = #paramSchema % #items
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe (SwaggerItems 'SwaggerKindParamOtherSchema)
  , b ~ Maybe (SwaggerItems 'SwaggerKindParamOtherSchema)
  ) => LabelOptic "items" A_Lens ParamOtherSchema ParamOtherSchema a b where
  labelOptic = #paramSchema % #items
  {-# INLINE labelOptic #-}

-- #maximum

instance
  ( a ~ Maybe Scientific, b ~ Maybe Scientific
  ) => LabelOptic "maximum" A_Lens Header Header a b where
  labelOptic = #paramSchema % #maximum
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Scientific, b ~ Maybe Scientific
  ) => LabelOptic "maximum" A_Lens Schema Schema a b where
  labelOptic = #paramSchema % #maximum
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Scientific, b ~ Maybe Scientific
  ) => LabelOptic "maximum" A_Lens NamedSchema NamedSchema a b where
  labelOptic = #paramSchema % #maximum
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Scientific, b ~ Maybe Scientific
  ) => LabelOptic "maximum" A_Lens ParamOtherSchema ParamOtherSchema a b where
  labelOptic = #paramSchema % #maximum
  {-# INLINE labelOptic #-}

-- #exclusiveMaximum

instance
  ( a ~ Maybe Bool, b ~ Maybe Bool
  ) => LabelOptic "exclusiveMaximum" A_Lens Header Header a b where
  labelOptic = #paramSchema % #exclusiveMaximum
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Bool, b ~ Maybe Bool
  ) => LabelOptic "exclusiveMaximum" A_Lens Schema Schema a b where
  labelOptic = #paramSchema % #exclusiveMaximum
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Bool, b ~ Maybe Bool
  ) => LabelOptic "exclusiveMaximum" A_Lens NamedSchema NamedSchema a b where
  labelOptic = #paramSchema % #exclusiveMaximum
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Bool, b ~ Maybe Bool
  ) => LabelOptic "exclusiveMaximum" A_Lens ParamOtherSchema ParamOtherSchema a b where
  labelOptic = #paramSchema % #exclusiveMaximum
  {-# INLINE labelOptic #-}

-- #minimum

instance
  ( a ~ Maybe Scientific, b ~ Maybe Scientific
  ) => LabelOptic "minimum" A_Lens Header Header a b where
  labelOptic = #paramSchema % #minimum
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Scientific, b ~ Maybe Scientific
  ) => LabelOptic "minimum" A_Lens Schema Schema a b where
  labelOptic = #paramSchema % #minimum
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Scientific, b ~ Maybe Scientific
  ) => LabelOptic "minimum" A_Lens NamedSchema NamedSchema a b where
  labelOptic = #paramSchema % #minimum
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Scientific, b ~ Maybe Scientific
  ) => LabelOptic "minimum" A_Lens ParamOtherSchema ParamOtherSchema a b where
  labelOptic = #paramSchema % #minimum
  {-# INLINE labelOptic #-}

-- #exclusiveMinimum

instance
  ( a ~ Maybe Bool, b ~ Maybe Bool
  ) => LabelOptic "exclusiveMinimum" A_Lens Header Header a b where
  labelOptic = #paramSchema % #exclusiveMinimum
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Bool, b ~ Maybe Bool
  ) => LabelOptic "exclusiveMinimum" A_Lens Schema Schema a b where
  labelOptic = #paramSchema % #exclusiveMinimum
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Bool, b ~ Maybe Bool
  ) => LabelOptic "exclusiveMinimum" A_Lens NamedSchema NamedSchema a b where
  labelOptic = #paramSchema % #exclusiveMinimum
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Bool, b ~ Maybe Bool
  ) => LabelOptic "exclusiveMinimum" A_Lens ParamOtherSchema ParamOtherSchema a b where
  labelOptic = #paramSchema % #exclusiveMinimum
  {-# INLINE labelOptic #-}

-- #maxLength

instance
  ( a ~ Maybe Integer, b ~ Maybe Integer
  ) => LabelOptic "maxLength" A_Lens Header Header a b where
  labelOptic = #paramSchema % #maxLength
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Integer, b ~ Maybe Integer
  ) => LabelOptic "maxLength" A_Lens Schema Schema a b where
  labelOptic = #paramSchema % #maxLength
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Integer, b ~ Maybe Integer
  ) => LabelOptic "maxLength" A_Lens NamedSchema NamedSchema a b where
  labelOptic = #paramSchema % #maxLength
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Integer, b ~ Maybe Integer
  ) => LabelOptic "maxLength" A_Lens ParamOtherSchema ParamOtherSchema a b where
  labelOptic = #paramSchema % #maxLength
  {-# INLINE labelOptic #-}

-- #minLength

instance
  ( a ~ Maybe Integer, b ~ Maybe Integer
  ) => LabelOptic "minLength" A_Lens Header Header a b where
  labelOptic = #paramSchema % #minLength
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Integer, b ~ Maybe Integer
  ) => LabelOptic "minLength" A_Lens Schema Schema a b where
  labelOptic = #paramSchema % #minLength
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Integer, b ~ Maybe Integer
  ) => LabelOptic "minLength" A_Lens NamedSchema NamedSchema a b where
  labelOptic = #paramSchema % #minLength
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Integer, b ~ Maybe Integer
  ) => LabelOptic "minLength" A_Lens ParamOtherSchema ParamOtherSchema a b where
  labelOptic = #paramSchema % #minLength
  {-# INLINE labelOptic #-}

-- #pattern

instance
  ( a ~ Maybe Text, b ~ Maybe Text
  ) => LabelOptic "pattern" A_Lens Header Header a b where
  labelOptic = #paramSchema % #pattern
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Text, b ~ Maybe Text
  ) => LabelOptic "pattern" A_Lens Schema Schema a b where
  labelOptic = #paramSchema % #pattern
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Text, b ~ Maybe Text
  ) => LabelOptic "pattern" A_Lens NamedSchema NamedSchema a b where
  labelOptic = #paramSchema % #pattern
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Text, b ~ Maybe Text
  ) => LabelOptic "pattern" A_Lens ParamOtherSchema ParamOtherSchema a b where
  labelOptic = #paramSchema % #pattern
  {-# INLINE labelOptic #-}

-- #maxItems

instance
  ( a ~ Maybe Integer, b ~ Maybe Integer
  ) => LabelOptic "maxItems" A_Lens Header Header a b where
  labelOptic = #paramSchema % #maxItems
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Integer, b ~ Maybe Integer
  ) => LabelOptic "maxItems" A_Lens Schema Schema a b where
  labelOptic = #paramSchema % #maxItems
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Integer, b ~ Maybe Integer
  ) => LabelOptic "maxItems" A_Lens NamedSchema NamedSchema a b where
  labelOptic = #paramSchema % #maxItems
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Integer, b ~ Maybe Integer
  ) => LabelOptic "maxItems" A_Lens ParamOtherSchema ParamOtherSchema a b where
  labelOptic = #paramSchema % #maxItems
  {-# INLINE labelOptic #-}

-- #minItems

instance
  ( a ~ Maybe Integer, b ~ Maybe Integer
  ) => LabelOptic "minItems" A_Lens Header Header a b where
  labelOptic = #paramSchema % #minItems
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Integer, b ~ Maybe Integer
  ) => LabelOptic "minItems" A_Lens Schema Schema a b where
  labelOptic = #paramSchema % #minItems
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Integer, b ~ Maybe Integer
  ) => LabelOptic "minItems" A_Lens NamedSchema NamedSchema a b where
  labelOptic = #paramSchema % #minItems
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Integer, b ~ Maybe Integer
  ) => LabelOptic "minItems" A_Lens ParamOtherSchema ParamOtherSchema a b where
  labelOptic = #paramSchema % #minItems
  {-# INLINE labelOptic #-}

-- #uniqueItems

instance
  ( a ~ Maybe Bool, b ~ Maybe Bool
  ) => LabelOptic "uniqueItems" A_Lens Header Header a b where
  labelOptic = #paramSchema % #uniqueItems
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Bool, b ~ Maybe Bool
  ) => LabelOptic "uniqueItems" A_Lens Schema Schema a b where
  labelOptic = #paramSchema % #uniqueItems
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Bool, b ~ Maybe Bool
  ) => LabelOptic "uniqueItems" A_Lens NamedSchema NamedSchema a b where
  labelOptic = #paramSchema % #uniqueItems
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Bool, b ~ Maybe Bool
  ) => LabelOptic "uniqueItems" A_Lens ParamOtherSchema ParamOtherSchema a b where
  labelOptic = #paramSchema % #uniqueItems
  {-# INLINE labelOptic #-}

-- #enum

instance
  ( a ~ Maybe [Value], b ~ Maybe [Value]
  ) => LabelOptic "enum" A_Lens Header Header a b where
  labelOptic = #paramSchema % #enum
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe [Value], b ~ Maybe [Value]
  ) => LabelOptic "enum" A_Lens Schema Schema a b where
  labelOptic = #paramSchema % #enum
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe [Value], b ~ Maybe [Value]
  ) => LabelOptic "enum" A_Lens NamedSchema NamedSchema a b where
  labelOptic = #paramSchema % #enum
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe [Value], b ~ Maybe [Value]
  ) => LabelOptic "enum" A_Lens ParamOtherSchema ParamOtherSchema a b where
  labelOptic = #paramSchema % #enum
  {-# INLINE labelOptic #-}

-- #multipleOf

instance
  ( a ~ Maybe Scientific, b ~ Maybe Scientific
  ) => LabelOptic "multipleOf" A_Lens Header Header a b where
  labelOptic = #paramSchema % #multipleOf
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Scientific, b ~ Maybe Scientific
  ) => LabelOptic "multipleOf" A_Lens Schema Schema a b where
  labelOptic = #paramSchema % #multipleOf
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Scientific, b ~ Maybe Scientific
  ) => LabelOptic "multipleOf" A_Lens NamedSchema NamedSchema a b where
  labelOptic = #paramSchema % #multipleOf
  {-# INLINE labelOptic #-}

instance
  ( a ~ Maybe Scientific, b ~ Maybe Scientific
  ) => LabelOptic "multipleOf" A_Lens ParamOtherSchema ParamOtherSchema a b where
  labelOptic = #paramSchema % #multipleOf
  {-# INLINE labelOptic #-}
