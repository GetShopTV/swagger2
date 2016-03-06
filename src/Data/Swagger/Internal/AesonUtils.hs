{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Swagger.Internal.AesonUtils (
    -- * Generic functions
    sopSwaggerGenericToJSON,
    -- * Options
    SwaggerAesonOptions,
    mkSwaggerAesonOptions,
    saoPrefix,
    saoAdditionalPairs,
    saoSubObject,
    ) where

import Prelude ()
import Prelude.Compat

import Control.Lens (makeLenses, (^.))
import Data.Aeson   (ToJSON(..), Value(..), object)
import Data.Text    (Text)
import Data.Char    (toLower, isUpper)

import Generics.SOP

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

-------------------------------------------------------------------------------
-- SwaggerAesonOptions
-------------------------------------------------------------------------------

data SwaggerAesonOptions = SwaggerAesonOptions
    { _saoPrefix          :: String
    , _saoAdditionalPairs :: [(Text, Value)]
    , _saoSubObject       :: Maybe String
    }

mkSwaggerAesonOptions
    :: String  -- ^ prefix
    -> SwaggerAesonOptions
mkSwaggerAesonOptions pfx = SwaggerAesonOptions pfx [] Nothing

makeLenses ''SwaggerAesonOptions

-------------------------------------------------------------------------------
-- Generics
-------------------------------------------------------------------------------

-- | Generic serialisation for swagger records.
--
-- Features
--
-- * omits nulls, empty objects and empty arrays (configurable)
-- * possible to add fields
-- * possible to merge sub-object
sopSwaggerGenericToJSON
    :: forall a xs. (Generic a, HasDatatypeInfo a, All2 ToJSON (Code a), Code a ~ '[xs])
    => SwaggerAesonOptions
    -> a
    -> Value
sopSwaggerGenericToJSON opts x =
    let ps = sopSwaggerGenericToJSON' opts (from x) (datatypeInfo (Proxy :: Proxy a))
    in object (opts ^. saoAdditionalPairs ++ ps)

sopSwaggerGenericToJSON' :: All2 ToJSON '[xs] => SwaggerAesonOptions -> SOP I '[xs] -> DatatypeInfo '[xs] -> [(Text, Value)]
sopSwaggerGenericToJSON' opts (SOP (Z fields)) (ADT _ _ (Record _ fieldsInfo :* Nil)) =
    sopSwaggerGenericToJSON'' opts fields fieldsInfo

sopSwaggerGenericToJSON'' :: All ToJSON xs => SwaggerAesonOptions -> NP I xs -> NP FieldInfo xs -> [(Text, Value)]
sopSwaggerGenericToJSON'' (SwaggerAesonOptions prefix _ sub) = go
  where
    go :: All ToJSON ys => NP I ys -> NP FieldInfo ys -> [(Text, Value)]
    go  Nil Nil = []
    go (I x :* xs) (FieldInfo name :* names)
        | Just name' == sub = case json of
              Object m -> HM.toList m ++ rest
              Null     -> rest
              _        -> error $ "sopSwaggerGenericToJSON: subjson is not an object: " ++ show json
        | json == Null || json == Array mempty || json == Object mempty =
            rest
        | otherwise =
            (T.pack name', json) : rest
      where
        json  = toJSON x
        name' = fieldNameModifier name
        rest  = go xs names

    fieldNameModifier = modifier . drop 1
    modifier = lowerFirstUppers . drop (length prefix)
    lowerFirstUppers s = map toLower x ++ y
      where (x, y) = span isUpper s
