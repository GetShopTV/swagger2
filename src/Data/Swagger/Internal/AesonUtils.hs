{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TemplateHaskell #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
module Data.Swagger.Internal.AesonUtils (
    -- * Generic functions
    AesonDefaultValue(..),
    sopSwaggerGenericToJSON,
#if MIN_VERSION_aeson(0,10,0)
    sopSwaggerGenericToEncoding,
#endif
    sopSwaggerGenericToJSONWithOpts,
    sopSwaggerGenericParseJSON,
    -- * Options
    HasSwaggerAesonOptions(..),
    SwaggerAesonOptions,
    mkSwaggerAesonOptions,
    saoPrefix,
    saoAdditionalPairs,
    saoSubObject,
    -- * Percent encoding
    percentEncodeS,
    percentEncodeT,
    percentDecodeS,
    percentDecodeT,
    ) where

import Prelude ()
import Prelude.Compat

import Control.Applicative ((<|>))
import Control.Lens     (makeLenses, (^.))
import Control.Monad    (unless)
import Data.Aeson       (ToJSON(..), FromJSON(..), Value(..), Object, object, (.:), (.:?), (.!=), withObject)
import Data.Aeson.Types (Parser, Pair)
import Data.Char        (toLower, isUpper)
import Data.Foldable    (traverse_)
import Data.Text        (Text)

import Generics.SOP

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import qualified Data.HashSet.InsOrd as InsOrdHS

#if MIN_VERSION_aeson(0,10,0)
import Data.Aeson (Encoding, pairs, (.=), Series)
#endif

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
    swaggerAesonOptions :: Proxy a -> SwaggerAesonOptions

    -- So far we use only default definitions
    aesonDefaults :: Proxy a -> POP Maybe (Code a)
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
instance AesonDefaultValue (InsOrdHS.InsOrdHashSet k) where defaultValue = Just InsOrdHS.empty
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
        ( HasDatatypeInfo a
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
    -> [Pair]
#if MIN_VERSION_generics_sop(0,5,0)
sopSwaggerGenericToJSON' opts (SOP (Z fields)) (ADT _ _ (Record _ fieldsInfo :* Nil) _) (POP (defs :* Nil)) =
#else
sopSwaggerGenericToJSON' opts (SOP (Z fields)) (ADT _ _ (Record _ fieldsInfo :* Nil)) (POP (defs :* Nil)) =
#endif
    sopSwaggerGenericToJSON'' opts fields fieldsInfo defs
sopSwaggerGenericToJSON' _ _ _ _ = error "sopSwaggerGenericToJSON: unsupported type"

sopSwaggerGenericToJSON''
    :: (All ToJSON xs, All Eq xs)
    => SwaggerAesonOptions
    -> NP I xs
    -> NP FieldInfo xs
    -> NP Maybe xs
    -> [Pair]
sopSwaggerGenericToJSON'' (SwaggerAesonOptions prefix _ sub) = go
  where
    go :: (All ToJSON ys, All Eq ys) => NP I ys -> NP FieldInfo ys -> NP Maybe ys -> [Pair]
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
#if __GLASGOW_HASKELL__ < 800
    go _ _ _ = error "not empty"
#endif

    fieldNameModifier = modifier . drop 1
    modifier = lowerFirstUppers . drop (length prefix)
    lowerFirstUppers s = map toLower x ++ y
      where (x, y) = span isUpper s

-------------------------------------------------------------------------------
-- FromJSON
-------------------------------------------------------------------------------

sopSwaggerGenericParseJSON
    :: forall a xs.
        ( HasDatatypeInfo a
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
        traverse_ (parseAdditionalField obj) (opts ^. saoAdditionalPairs)
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
#if MIN_VERSION_generics_sop(0,5,0)
sopSwaggerGenericParseJSON' opts obj (ADT _ _ (Record _ fieldsInfo :* Nil) _) (POP (defs :* Nil)) =
#else
sopSwaggerGenericParseJSON' opts obj (ADT _ _ (Record _ fieldsInfo :* Nil)) (POP (defs :* Nil)) =
#endif
    SOP . Z <$> sopSwaggerGenericParseJSON'' opts obj fieldsInfo defs
sopSwaggerGenericParseJSON' _ _ _ _ = error "sopSwaggerGenericParseJSON: unsupported type"

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
#if __GLASGOW_HASKELL__ < 800
    go _ _ = error "not empty"
#endif

    fieldNameModifier = modifier . drop 1
    modifier = lowerFirstUppers . drop (length prefix)
    lowerFirstUppers s = map toLower x ++ y
      where (x, y) = span isUpper s

-------------------------------------------------------------------------------
-- ToEncoding
-------------------------------------------------------------------------------

#if MIN_VERSION_aeson(0,10,0)

sopSwaggerGenericToEncoding
    :: forall a xs.
        ( HasDatatypeInfo a
        , HasSwaggerAesonOptions a
        , All2 ToJSON (Code a)
        , All2 Eq (Code a)
        , Code a ~ '[xs]
        )
    => a
    -> Encoding
sopSwaggerGenericToEncoding x =
    let ps = sopSwaggerGenericToEncoding' opts (from x) (datatypeInfo proxy) (aesonDefaults proxy)
    in pairs (pairsToSeries (opts ^. saoAdditionalPairs) <> ps)
  where
    proxy = Proxy :: Proxy a
    opts  = swaggerAesonOptions proxy

pairsToSeries :: [Pair] -> Series
pairsToSeries = foldMap (\(k, v) -> (k .= v))

sopSwaggerGenericToEncoding'
    :: (All2 ToJSON '[xs], All2 Eq '[xs])
    => SwaggerAesonOptions
    -> SOP I '[xs]
    -> DatatypeInfo '[xs]
    -> POP Maybe '[xs]
    -> Series
#if MIN_VERSION_generics_sop(0,5,0)
sopSwaggerGenericToEncoding' opts (SOP (Z fields)) (ADT _ _ (Record _ fieldsInfo :* Nil) _) (POP (defs :* Nil)) =
#else
sopSwaggerGenericToEncoding' opts (SOP (Z fields)) (ADT _ _ (Record _ fieldsInfo :* Nil)) (POP (defs :* Nil)) =
#endif
    sopSwaggerGenericToEncoding'' opts fields fieldsInfo defs
sopSwaggerGenericToEncoding' _ _ _ _ = error "sopSwaggerGenericToEncoding: unsupported type"

sopSwaggerGenericToEncoding''
    :: (All ToJSON xs, All Eq xs)
    => SwaggerAesonOptions
    -> NP I xs
    -> NP FieldInfo xs
    -> NP Maybe xs
    -> Series
sopSwaggerGenericToEncoding'' (SwaggerAesonOptions prefix _ sub) = go
  where
    go :: (All ToJSON ys, All Eq ys) => NP I ys -> NP FieldInfo ys -> NP Maybe ys -> Series
    go  Nil Nil Nil = mempty
    go (I x :* xs) (FieldInfo name :* names) (def :* defs)
        | Just name' == sub = case toJSON x of
              Object m -> pairsToSeries (HM.toList m) <> rest
              Null     -> rest
              _        -> error $ "sopSwaggerGenericToJSON: subjson is not an object: " ++ show (toJSON x)
        -- If default value: omit it.
        | Just x == def =
            rest
        | otherwise =
            (T.pack name' .= x) <> rest
      where
        name' = fieldNameModifier name
        rest  = go xs names defs
#if __GLASGOW_HASKELL__ < 800
    go _ _ _ = error "not empty"
#endif

    fieldNameModifier = modifier . drop 1
    modifier = lowerFirstUppers . drop (length prefix)
    lowerFirstUppers s = map toLower x ++ y
      where (x, y) = span isUpper s

#endif

percentEncodeS :: String -> String
percentEncodeS "" = ""
percentEncodeS ('!' : s) = "%21" <> percentEncodeS s
percentEncodeS ('#' : s) = "%23" <> percentEncodeS s
percentEncodeS ('$' : s) = "%24" <> percentEncodeS s
percentEncodeS ('%' : s) = "%25" <> percentEncodeS s
percentEncodeS ('&' : s) = "%26" <> percentEncodeS s
percentEncodeS ('\'' : s) = "%27" <> percentEncodeS s
percentEncodeS ('(' : s) = "%28" <> percentEncodeS s
percentEncodeS (')' : s) = "%29" <> percentEncodeS s
percentEncodeS ('*' : s) = "%2A" <> percentEncodeS s
percentEncodeS ('+' : s) = "%2B" <> percentEncodeS s
percentEncodeS (',' : s) = "%2C" <> percentEncodeS s
percentEncodeS ('/' : s) = "%2F" <> percentEncodeS s
percentEncodeS (':' : s) = "%3A" <> percentEncodeS s
percentEncodeS (';' : s) = "%3B" <> percentEncodeS s
percentEncodeS ('=' : s) = "%3D" <> percentEncodeS s
percentEncodeS ('?' : s) = "%3F" <> percentEncodeS s
percentEncodeS ('@' : s) = "%40" <> percentEncodeS s
percentEncodeS ('[' : s) = "%5B" <> percentEncodeS s
percentEncodeS (']' : s) = "%5D" <> percentEncodeS s
percentEncodeS (c : s) = c : percentEncodeS s

percentEncodeT :: Text -> Text
percentEncodeT = T.pack . percentEncodeS . T.unpack

percentDecodeS :: String -> String
percentDecodeS "" = ""
percentDecodeS ('%':'2':'1':s) = '!' : percentDecodeS s
percentDecodeS ('%':'2':'3':s) = '#' : percentDecodeS s
percentDecodeS ('%':'2':'4':s) = '$' : percentDecodeS s
percentDecodeS ('%':'2':'5':s) = '%' : percentDecodeS s
percentDecodeS ('%':'2':'6':s) = '&' : percentDecodeS s
percentDecodeS ('%':'2':'7':s) = '\'' : percentDecodeS s
percentDecodeS ('%':'2':'8':s) = '(' : percentDecodeS s
percentDecodeS ('%':'2':'9':s) = ')' : percentDecodeS s
percentDecodeS ('%':'2':'A':s) = '*' : percentDecodeS s
percentDecodeS ('%':'2':'B':s) = '+' : percentDecodeS s
percentDecodeS ('%':'2':'C':s) = ',' : percentDecodeS s
percentDecodeS ('%':'2':'F':s) = '/' : percentDecodeS s
percentDecodeS ('%':'3':'A':s) = ':' : percentDecodeS s
percentDecodeS ('%':'3':'B':s) = ';' : percentDecodeS s
percentDecodeS ('%':'3':'D':s) = '=' : percentDecodeS s
percentDecodeS ('%':'3':'F':s) = '?' : percentDecodeS s
percentDecodeS ('%':'4':'0':s) = '@' : percentDecodeS s
percentDecodeS ('%':'5':'B':s) = '[' : percentDecodeS s
percentDecodeS ('%':'5':'D':s) = ']' : percentDecodeS s
percentDecodeS (c : s) = c : percentDecodeS s

percentDecodeT :: Text -> Text
percentDecodeT = T.pack . percentDecodeS . T.unpack
