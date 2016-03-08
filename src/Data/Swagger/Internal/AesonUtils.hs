{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Swagger.Internal.AesonUtils (
    -- * Generic functions
    AesonDefaultValue(..),
    sopSwaggerGenericToJSON,
    sopSwaggerGenericToJSONWithOpts,
    sopSwaggerGenericParseJSON,
    -- * Options
    HasSwaggerAesonOptions(..),
    SwaggerAesonOptions,
    mkSwaggerAesonOptions,
    saoPrefix,
    saoAdditionalPairs,
    saoSubObject,
    ) where

import Prelude ()
import Prelude.Compat

import Control.Applicative ((<|>))
import Control.Lens     (makeLenses, (^.))
import Control.Monad    (unless)
import Data.Aeson       (ToJSON(..), FromJSON(..), Value(..), Object, object, (.:), (.:?), (.!=), withObject)
import Data.Aeson.Types (Parser)
import Data.Char        (toLower, isUpper)
import Data.Text        (Text)

import Generics.SOP

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.HashMap.Strict.InsOrd as InsOrd

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

class (Generic a, All2 AesonDefaultValue (Code a)) => HasSwaggerAesonOptions a where
    swaggerAesonOptions :: proxy a -> SwaggerAesonOptions

    -- So far we use only default definitions
    aesonDefaults :: proxy a -> POP Maybe (Code a)
    aesonDefaults _ = hcpure (Proxy :: Proxy AesonDefaultValue) defaultValue

-------------------------------------------------------------------------------
-- Generics
-------------------------------------------------------------------------------

class AesonDefaultValue a where
    defaultValue :: Maybe a
    defaultValue = Nothing

instance AesonDefaultValue Text where defaultValue = Nothing
instance AesonDefaultValue (Maybe a) where defaultValue = Just Nothing
instance AesonDefaultValue [a] where defaultValue = Just []
instance AesonDefaultValue (Set.Set a) where defaultValue = Just Set.empty
instance AesonDefaultValue (InsOrd.InsOrdHashMap k v) where defaultValue = Just InsOrd.empty

-------------------------------------------------------------------------------
-- ToJSON
-------------------------------------------------------------------------------

-- | Generic serialisation for swagger records.
--
-- Features
--
-- * omits nulls, empty objects and empty arrays (configurable)
-- * possible to add fields
-- * possible to merge sub-object
sopSwaggerGenericToJSON
    :: forall a xs.
        ( Generic a
        , HasDatatypeInfo a
        , HasSwaggerAesonOptions a
        , All2 ToJSON (Code a)
        , All2 Eq (Code a)
        , Code a ~ '[xs]
        )
    => a
    -> Value
sopSwaggerGenericToJSON x =
    let ps = sopSwaggerGenericToJSON' opts (from x) (datatypeInfo proxy) (aesonDefaults proxy)
    in object (opts ^. saoAdditionalPairs ++ ps)
  where
    proxy = Proxy :: Proxy a
    opts  = swaggerAesonOptions proxy

-- | *TODO:* This is only used by ToJSON (ParamSchema SwaggerKindSchema)
--
-- Also uses default `aesonDefaults`
sopSwaggerGenericToJSONWithOpts
    :: forall a xs.
        ( Generic a
        , All2 AesonDefaultValue (Code a)
        , HasDatatypeInfo a
        , All2 ToJSON (Code a)
        , All2 Eq (Code a)
        , Code a ~ '[xs]
        )
    => SwaggerAesonOptions
    -> a
    -> Value
sopSwaggerGenericToJSONWithOpts opts x =
    let ps = sopSwaggerGenericToJSON' opts (from x) (datatypeInfo proxy) defs
    in object (opts ^. saoAdditionalPairs ++ ps)
  where
    proxy = Proxy :: Proxy a
    defs = hcpure (Proxy :: Proxy AesonDefaultValue) defaultValue

sopSwaggerGenericToJSON'
    :: (All2 ToJSON '[xs], All2 Eq '[xs])
    => SwaggerAesonOptions
    -> SOP I '[xs]
    -> DatatypeInfo '[xs]
    -> POP Maybe '[xs]
    -> [(Text, Value)]
sopSwaggerGenericToJSON' opts (SOP (Z fields)) (ADT _ _ (Record _ fieldsInfo :* Nil)) (POP (defs :* Nil)) =
    sopSwaggerGenericToJSON'' opts fields fieldsInfo defs

sopSwaggerGenericToJSON''
    :: (All ToJSON xs, All Eq xs)
    => SwaggerAesonOptions
    -> NP I xs
    -> NP FieldInfo xs
    -> NP Maybe xs
    -> [(Text, Value)]
sopSwaggerGenericToJSON'' (SwaggerAesonOptions prefix _ sub) = go
  where
    go :: (All ToJSON ys, All Eq ys) => NP I ys -> NP FieldInfo ys -> NP Maybe ys -> [(Text, Value)]
    go  Nil Nil Nil = []
    go (I x :* xs) (FieldInfo name :* names) (def :* defs)
        | Just name' == sub = case json of
              Object m -> HM.toList m ++ rest
              Null     -> rest
              _        -> error $ "sopSwaggerGenericToJSON: subjson is not an object: " ++ show json
        -- If default value: omit it.
        | Just x == def =
            rest
        | otherwise =
            (T.pack name', json) : rest
      where
        json  = toJSON x
        name' = fieldNameModifier name
        rest  = go xs names defs

    fieldNameModifier = modifier . drop 1
    modifier = lowerFirstUppers . drop (length prefix)
    lowerFirstUppers s = map toLower x ++ y
      where (x, y) = span isUpper s

-------------------------------------------------------------------------------
-- FromJSON
-------------------------------------------------------------------------------

sopSwaggerGenericParseJSON
    :: forall a xs.
        ( Generic a
        , HasDatatypeInfo a
        , HasSwaggerAesonOptions a
        , All2 FromJSON (Code a)
        , All2 Eq (Code a)
        , Code a ~ '[xs]
        )
    => Value
    -> Parser a
sopSwaggerGenericParseJSON = withObject "Swagger Record Object" $ \obj ->
    let ps = sopSwaggerGenericParseJSON' opts obj (datatypeInfo proxy) (aesonDefaults proxy)
    in do
        traverse (parseAdditionalField obj) (opts ^. saoAdditionalPairs)
        to <$> ps
  where
    proxy = Proxy :: Proxy a
    opts  = swaggerAesonOptions proxy

    parseAdditionalField :: Object -> (Text, Value) -> Parser ()
    parseAdditionalField obj (k, v) = do
        v' <- obj .: k
        unless (v == v') $ fail $
            "Additonal field don't match for key " ++ T.unpack k
            ++ ": " ++ show v
            ++ " /= " ++ show v'

sopSwaggerGenericParseJSON'
    :: (All2 FromJSON '[xs], All2 Eq '[xs])
    => SwaggerAesonOptions
    -> Object
    -> DatatypeInfo '[xs]
    -> POP Maybe '[xs]
    -> Parser (SOP I '[xs])
sopSwaggerGenericParseJSON' opts obj (ADT _ _ (Record _ fieldsInfo :* Nil)) (POP (defs :* Nil)) =
    SOP . Z <$> sopSwaggerGenericParseJSON'' opts obj fieldsInfo defs

sopSwaggerGenericParseJSON''
    :: (All FromJSON xs, All Eq xs)
    => SwaggerAesonOptions
    -> Object
    -> NP FieldInfo xs
    -> NP Maybe xs
    -> Parser (NP I xs)
sopSwaggerGenericParseJSON'' (SwaggerAesonOptions prefix _ sub) obj = go
  where
    go :: (All FromJSON ys, All Eq ys) => NP FieldInfo ys -> NP Maybe ys -> Parser (NP I ys)
    go  Nil Nil = pure Nil
    go (FieldInfo name :* names) (def :* defs)
        | Just name' == sub =
            -- Note: we might strip fields of outer structure.
            cons <$> (withDef $ parseJSON $ Object obj) <*> rest
        | otherwise = case def of
            Just def' -> cons <$> obj .:? T.pack name' .!= def' <*> rest
            Nothing  ->  cons <$> obj .: T.pack name' <*> rest
      where
        cons h t = I h :* t
        name' = fieldNameModifier name
        rest  = go names defs

        withDef = case def of
            Just def' -> (<|> pure def')
            Nothing   -> id

    fieldNameModifier = modifier . drop 1
    modifier = lowerFirstUppers . drop (length prefix)
    lowerFirstUppers s = map toLower x ++ y
      where (x, y) = span isUpper s
