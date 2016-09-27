{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#include "overlapping-compat.h"
module Data.Swagger.Internal.ParamSchema where

import Control.Lens
import Data.Aeson
import Data.Proxy
import GHC.Generics

import Data.Int
import "unordered-containers" Data.HashSet (HashSet)
import Data.Monoid
import Data.Set (Set)
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Data.UUID.Types (UUID)

import Data.Swagger.Internal
import Data.Swagger.Lens
import Data.Swagger.SchemaOptions

#if __GLASGOW_HASKELL__ < 800
#else
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import GHC.TypeLits (TypeError, ErrorMessage(..))
#endif

-- | Default schema for binary data (any sequence of octets).
binaryParamSchema :: ParamSchema t
binaryParamSchema = mempty
  & type_ .~ SwaggerString
  & format ?~ "binary"

-- | Default schema for binary data (base64 encoded).
byteParamSchema :: ParamSchema t
byteParamSchema = mempty
  & type_ .~ SwaggerString
  & format ?~ "byte"

-- | Default schema for password string.
-- @"password"@ format is used to hint UIs the input needs to be obscured.
passwordParamSchema :: ParamSchema t
passwordParamSchema = mempty
  & type_ .~ SwaggerString
  & format ?~ "password"

-- | Convert a type into a plain @'ParamSchema'@.
--
-- An example type and instance:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}   -- allows to write 'T.Text' literals
--
-- import Control.Lens
--
-- data Direction = Up | Down
--
-- instance ToParamSchema Direction where
--   toParamSchema = mempty
--      & type_ .~ SwaggerString
--      & enum_ .~ [ \"Up\", \"Down\" ]
-- @
--
-- Instead of manually writing your @'ToParamSchema'@ instance you can
-- use a default generic implementation of @'toParamSchema'@.
--
-- To do that, simply add @deriving 'Generic'@ clause to your datatype
-- and declare a @'ToParamSchema'@ instance for your datatype without
-- giving definition for @'toParamSchema'@.
--
-- For instance, the previous example can be simplified into this:
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import GHC.Generics (Generic)
--
-- data Direction = Up | Down deriving Generic
--
-- instance ToParamSchema Direction
-- @
class ToParamSchema a where
  -- | Convert a type into a plain parameter schema.
  --
  -- >>> encode $ toParamSchema (Proxy :: Proxy Integer)
  -- "{\"type\":\"integer\"}"
  toParamSchema :: proxy a -> ParamSchema t
  default toParamSchema :: (Generic a, GToParamSchema (Rep a)) => proxy a -> ParamSchema t
  toParamSchema = genericToParamSchema defaultSchemaOptions

instance OVERLAPPING_ ToParamSchema String where
  toParamSchema _ = mempty & type_ .~ SwaggerString

instance ToParamSchema Bool where
  toParamSchema _ = mempty & type_ .~ SwaggerBoolean

instance ToParamSchema Integer where
  toParamSchema _ = mempty & type_ .~ SwaggerInteger

instance ToParamSchema Int    where toParamSchema = toParamSchemaBoundedIntegral
instance ToParamSchema Int8   where toParamSchema = toParamSchemaBoundedIntegral
instance ToParamSchema Int16  where toParamSchema = toParamSchemaBoundedIntegral

instance ToParamSchema Int32 where
  toParamSchema proxy = toParamSchemaBoundedIntegral proxy & format ?~ "int32"

instance ToParamSchema Int64 where
  toParamSchema proxy = toParamSchemaBoundedIntegral proxy & format ?~ "int64"

instance ToParamSchema Word   where toParamSchema = toParamSchemaBoundedIntegral
instance ToParamSchema Word8  where toParamSchema = toParamSchemaBoundedIntegral
instance ToParamSchema Word16 where toParamSchema = toParamSchemaBoundedIntegral
instance ToParamSchema Word32 where toParamSchema = toParamSchemaBoundedIntegral
instance ToParamSchema Word64 where toParamSchema = toParamSchemaBoundedIntegral

-- | Default plain schema for @'Bounded'@, @'Integral'@ types.
--
-- >>> encode $ toParamSchemaBoundedIntegral (Proxy :: Proxy Int8)
-- "{\"maximum\":127,\"minimum\":-128,\"type\":\"integer\"}"
toParamSchemaBoundedIntegral :: forall proxy a t. (Bounded a, Integral a) => proxy a -> ParamSchema t
toParamSchemaBoundedIntegral _ = mempty
  & type_ .~ SwaggerInteger
  & minimum_ ?~ fromInteger (toInteger (minBound :: a))
  & maximum_ ?~ fromInteger (toInteger (maxBound :: a))

instance ToParamSchema Char where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString
    & maxLength ?~ 1
    & minLength ?~ 1

instance ToParamSchema Scientific where
  toParamSchema _ = mempty & type_ .~ SwaggerNumber

instance ToParamSchema Double where
  toParamSchema _ = mempty
    & type_  .~ SwaggerNumber
    & format ?~ "double"

instance ToParamSchema Float where
  toParamSchema _ = mempty
    & type_  .~ SwaggerNumber
    & format ?~ "float"

timeParamSchema :: String -> ParamSchema t
timeParamSchema fmt = mempty
  & type_  .~ SwaggerString
  & format ?~ T.pack fmt

-- | Format @"date"@ corresponds to @yyyy-mm-dd@ format.
instance ToParamSchema Day where
  toParamSchema _ = timeParamSchema "date"

-- |
-- >>> toParamSchema (Proxy :: Proxy LocalTime) ^. format
-- Just "yyyy-mm-ddThh:MM:ss"
instance ToParamSchema LocalTime where
  toParamSchema _ = timeParamSchema "yyyy-mm-ddThh:MM:ss"

-- |
-- >>> toParamSchema (Proxy :: Proxy ZonedTime) ^. format
-- Just "yyyy-mm-ddThh:MM:ss+hhMM"
instance ToParamSchema ZonedTime where
  toParamSchema _ = timeParamSchema "yyyy-mm-ddThh:MM:ss+hhMM"

-- |
-- >>> toParamSchema (Proxy :: Proxy UTCTime) ^. format
-- Just "yyyy-mm-ddThh:MM:ssZ"
instance ToParamSchema UTCTime where
  toParamSchema _ = timeParamSchema "yyyy-mm-ddThh:MM:ssZ"

instance ToParamSchema NominalDiffTime where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Integer)

instance ToParamSchema T.Text where
  toParamSchema _ = toParamSchema (Proxy :: Proxy String)

instance ToParamSchema TL.Text where
  toParamSchema _ = toParamSchema (Proxy :: Proxy String)

#if __GLASGOW_HASKELL__ < 800
#else
type family ToParamSchemaByteStringError bs where
  ToParamSchemaByteStringError bs = TypeError
      ( Text "Impossible to have an instance " :<>: ShowType (ToParamSchema bs) :<>: Text "."
   :$$: Text "Please, use a newtype wrapper around " :<>: ShowType bs :<>: Text " instead."
   :$$: Text "Consider using byteParamSchema or binaryParamSchema templates." )

instance ToParamSchemaByteStringError BS.ByteString  => ToParamSchema BS.ByteString  where toParamSchema = error "impossible"
instance ToParamSchemaByteStringError BSL.ByteString => ToParamSchema BSL.ByteString where toParamSchema = error "impossible"
#endif

instance ToParamSchema All where toParamSchema _ = toParamSchema (Proxy :: Proxy Bool)
instance ToParamSchema Any where toParamSchema _ = toParamSchema (Proxy :: Proxy Bool)
instance ToParamSchema a => ToParamSchema (Sum a)     where toParamSchema _ = toParamSchema (Proxy :: Proxy a)
instance ToParamSchema a => ToParamSchema (Product a) where toParamSchema _ = toParamSchema (Proxy :: Proxy a)
instance ToParamSchema a => ToParamSchema (First a)   where toParamSchema _ = toParamSchema (Proxy :: Proxy a)
instance ToParamSchema a => ToParamSchema (Last a)    where toParamSchema _ = toParamSchema (Proxy :: Proxy a)
instance ToParamSchema a => ToParamSchema (Dual a)    where toParamSchema _ = toParamSchema (Proxy :: Proxy a)

instance ToParamSchema a => ToParamSchema [a] where
  toParamSchema _ = mempty
    & type_ .~ SwaggerArray
    & items ?~ SwaggerItemsPrimitive Nothing (toParamSchema (Proxy :: Proxy a))

instance ToParamSchema a => ToParamSchema (V.Vector a) where toParamSchema _ = toParamSchema (Proxy :: Proxy [a])
instance ToParamSchema a => ToParamSchema (VP.Vector a) where toParamSchema _ = toParamSchema (Proxy :: Proxy [a])
instance ToParamSchema a => ToParamSchema (VS.Vector a) where toParamSchema _ = toParamSchema (Proxy :: Proxy [a])
instance ToParamSchema a => ToParamSchema (VU.Vector a) where toParamSchema _ = toParamSchema (Proxy :: Proxy [a])

instance ToParamSchema a => ToParamSchema (Set a) where
  toParamSchema _ = toParamSchema (Proxy :: Proxy [a])
    & uniqueItems ?~ True

instance ToParamSchema a => ToParamSchema (HashSet a) where
  toParamSchema _ = toParamSchema (Proxy :: Proxy (Set a))

-- |
-- >>> encode $ toParamSchema (Proxy :: Proxy ())
-- "{\"type\":\"string\",\"enum\":[\"_\"]}"
instance ToParamSchema () where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString
    & enum_ ?~ ["_"]

instance ToParamSchema UUID where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString
    & format ?~ "uuid"

-- | A configurable generic @'ParamSchema'@ creator.
--
-- >>> :set -XDeriveGeneric
-- >>> data Color = Red | Blue deriving Generic
-- >>> encode $ genericToParamSchema defaultSchemaOptions (Proxy :: Proxy Color)
-- "{\"type\":\"string\",\"enum\":[\"Red\",\"Blue\"]}"
genericToParamSchema :: forall proxy a t. (Generic a, GToParamSchema (Rep a)) => SchemaOptions -> proxy a -> ParamSchema t
genericToParamSchema opts _ = gtoParamSchema opts (Proxy :: Proxy (Rep a)) mempty

class GToParamSchema (f :: * -> *) where
  gtoParamSchema :: SchemaOptions -> proxy f -> ParamSchema t -> ParamSchema t

instance GToParamSchema f => GToParamSchema (D1 d f) where
  gtoParamSchema opts _ = gtoParamSchema opts (Proxy :: Proxy f)

instance Constructor c => GToParamSchema (C1 c U1) where
  gtoParamSchema = genumParamSchema

instance GToParamSchema f => GToParamSchema (C1 c (S1 s f)) where
  gtoParamSchema opts _ = gtoParamSchema opts (Proxy :: Proxy f)

instance ToParamSchema c => GToParamSchema (K1 i c) where
  gtoParamSchema _ _ _ = toParamSchema (Proxy :: Proxy c)

instance (GEnumParamSchema f, GEnumParamSchema g) => GToParamSchema (f :+: g) where
  gtoParamSchema opts _ = genumParamSchema opts (Proxy :: Proxy (f :+: g))

class GEnumParamSchema (f :: * -> *) where
  genumParamSchema :: SchemaOptions -> proxy f -> ParamSchema t -> ParamSchema t

instance (GEnumParamSchema f, GEnumParamSchema g) => GEnumParamSchema (f :+: g) where
  genumParamSchema opts _ = genumParamSchema opts (Proxy :: Proxy f) . genumParamSchema opts (Proxy :: Proxy g)

instance Constructor c => GEnumParamSchema (C1 c U1) where
  genumParamSchema opts _ s = s
    & type_ .~ SwaggerString
    & enum_ %~ addEnumValue tag
    where
      tag = toJSON (constructorTagModifier opts (conName (Proxy3 :: Proxy3 c f p)))

      addEnumValue x Nothing    = Just [x]
      addEnumValue x (Just xs)  = Just (x:xs)

data Proxy3 a b c = Proxy3

