{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module:      Data.Swagger.Schema.Validate
-- Copyright:   (c) 2015 GetShopTV
-- License:     BSD3
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Validate JSON values with Swagger Schema.
module Data.Swagger.Schema.Validation where

import Control.Applicative
import Control.Lens
import Control.Lens.TH
import Control.Monad (when)

import Data.Aeson hiding (Result)
import Data.Foldable (traverse_, for_, sequenceA_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified "unordered-containers" Data.HashSet as HashSet
import Data.Monoid
import Data.Scientific (Scientific, isInteger)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Swagger.Internal
import Data.Swagger.Lens


data Result a
  = Failed [String]
  | Passed a
  deriving (Eq, Show, Functor)

instance Applicative Result where
  pure = Passed
  Passed f <*> Passed x = Passed (f x)
  Failed xs <*> Failed ys = Failed (xs <> ys)
  Failed xs <*> _ = Failed xs
  _ <*> Failed ys = Failed ys

instance Alternative Result where
  empty = Failed mempty
  Passed x <|> _ = Passed x
  _        <|> y = y

instance Monad Result where
  return = pure
  Passed x >>=  f = f x
  Failed xs >>= f = Failed xs

data Config = Config
  { configPatternChecker :: Pattern -> Text -> Bool
  , configDefinitions    :: Definitions Schema
  }

-- | Default @'Config'@:
--
-- @
-- defaultConfig = 'Config'
--   { 'configPatternChecker' = \_pattern _str -> True
--   , 'configDefinitions'    = mempty
--   }
-- @
defaultConfig :: Config
defaultConfig = Config
  { configPatternChecker = \_pattern _str -> True
  , configDefinitions    = mempty
  }

newtype Validation s a = Validation { runValidation :: Config -> s -> Result a }
  deriving (Functor)

instance Applicative (Validation schema) where
  pure x = Validation (\_ _ -> pure x)
  Validation f <*> Validation x = Validation (\c s -> f c s <*> x c s)

instance Alternative (Validation schema) where
  empty = Validation (\_ _ -> empty)
  Validation x <|> Validation y = Validation (\c s -> x c s <|> y c s)

instance Profunctor Validation where
  dimap f g (Validation k) = Validation (\c s -> fmap g (k c (f s)))

instance Choice Validation where
  left'  (Validation g) = Validation (\c -> either (fmap Left . g c) (pure . Right))
  right' (Validation g) = Validation (\c -> either (pure . Left) (fmap Right . g c))

instance Monad (Validation s) where
  return = pure
  Validation x >>= f = Validation (\c s -> x c s >>= \x -> runValidation (f x) c s)
  (>>) = (*>)

withConfig :: (Config -> Validation s a) -> Validation s a
withConfig f = Validation (\c -> runValidation (f c) c)

withSchema :: (s -> Validation s a) -> Validation s a
withSchema f = Validation (\c s -> runValidation (f s) c s)

invalid :: String -> Validation schema a
invalid msg = Validation (\_ _ -> Failed [msg])

valid :: Validation schema ()
valid = pure ()

check :: Lens' s (Maybe a) -> (a -> Validation s ()) -> Validation s ()
check l g = withSchema $ \schema ->
  case schema ^. l of
    Nothing -> valid
    Just x  -> g x

sub :: t -> Validation t a -> Validation s a
sub = lmap . const

sub_ :: Getting a s a -> Validation a r -> Validation s r
sub_ = lmap . view

withRef :: Reference -> (Schema -> Validation s a) -> Validation s a
withRef (Reference ref) f = withConfig $ \cfg ->
  case HashMap.lookup ref (configDefinitions cfg) of
    Nothing -> invalid $ "unknown schema " ++ show ref
    Just s  -> f s

validateWithSchemaRef :: Referenced Schema -> Value -> Validation s ()
validateWithSchemaRef (Ref ref)  js = withRef ref $ \schema -> sub schema (validateWithSchema js)
validateWithSchemaRef (Inline s) js = sub s (validateWithSchema js)

validateWithSchema :: Value -> Validation Schema ()
validateWithSchema value = do
  validateSchemaType value
  sub_ paramSchema $ validateEnum value

validateWithParamSchema :: Value -> Validation (ParamSchema t) ()
validateWithParamSchema value = do
  validateParamSchemaType value
  validateEnum value

validateInteger :: Scientific -> Validation (ParamSchema t) ()
validateInteger n = do
  when (not (isInteger n)) $
    invalid ("not an integer: " ++ show n)
  validateNumber n

validateNumber :: Scientific -> Validation (ParamSchema t) ()
validateNumber n = withConfig $ \cfg -> withSchema $ \schema -> do
  let exMax = Just True == schema ^. exclusiveMaximum
      exMin = Just True == schema ^. exclusiveMinimum

  check maximum_ $ \m ->
    when (if exMax then (n >= m) else (n > m)) $
      invalid ("value " ++ show n ++ " exceeds maximum (should be " ++ if exMax then "<" else "<=" ++ show m ++ ")")

  check minimum_ $ \m ->
    when (if exMin then (n <= m) else (n < m)) $
      invalid ("value " ++ show n ++ " falls below minimum (should be " ++ if exMin then ">" else ">=" ++ show m ++ ")")

  check multipleOf $ \k ->
    when (not (isInteger (n / k))) $
      invalid ("expected a multiple of " ++ show k ++ " but got " ++ show n)

validateString :: Text -> Validation (ParamSchema t) ()
validateString s = do
  check maxLength $ \n ->
    when (len > fromInteger n) $
      invalid ("string is too long (length should be <=" ++ show n ++ "): " ++ show s)

  check minLength $ \n ->
    when (len < fromInteger n) $
      invalid ("string is too short (length should be >=" ++ show n ++ "): " ++ show s)

  check pattern $ \regex -> do
    withConfig $ \cfg -> do
      when (not (configPatternChecker cfg regex s)) $
        invalid ("string does not match pattern " ++ show regex ++ ": " ++ show s)
  where
    len = Text.length s

validateArray :: Vector Value -> Validation (ParamSchema t) ()
validateArray xs = do
  check maxItems $ \n ->
    when (len > fromInteger n) $
      invalid ("array exceeds maximum size (should be <=" ++ show n ++ "): " ++ show (encode xs))

  check minItems $ \n ->
    when (len < fromInteger n) $
      invalid ("array is too short (size should be >=" ++ show n ++ "): " ++ show (encode xs))

  check items $ \case
    SwaggerItemsPrimitive _ itemSchema -> sub itemSchema $ traverse_ validateWithParamSchema xs
    SwaggerItemsObject itemSchema      -> traverse_ (validateWithSchemaRef itemSchema) xs
    SwaggerItemsArray itemSchemas -> do
      when (len /= length itemSchemas) $
        invalid ("array size is invalid (should be exactly " ++ show (length itemSchemas) ++ "): " ++ show (encode xs))
      sequenceA_ (zipWith validateWithSchemaRef itemSchemas (Vector.toList xs))

  check uniqueItems $ \unique ->
    when (unique && not allUnique) $
      invalid ("array is expected to contain unique items, but it does not: " ++ show (encode xs))
  where
    len = Vector.length xs
    allUnique = len == HashSet.size (HashSet.fromList (Vector.toList xs))

validateObject :: HashMap Text Value -> Validation Schema ()
validateObject o = withSchema $ \schema ->
  case schema ^. discriminator of
    Just pname -> case fromJSON <$> HashMap.lookup pname o of
      Just (Success ref) -> validateWithSchemaRef ref (Object o)
      Just (Error msg)   -> invalid ("failed to parse discriminator property " ++ show pname ++ ": " ++ show msg)
      Nothing            -> invalid ("discriminator property " ++ show pname ++ "is missing: " ++ show (encode o))
    Nothing -> do
      check maxProperties $ \n ->
        when (size > n) $
          invalid ("object size exceeds maximum (total number of properties should be <=" ++ show n ++ "): " ++ show (encode o))

      check minProperties $ \n ->
        when (size < n) $
          invalid ("object size is too small (total number of properties should be >=" ++ show n ++ "): " ++ show (encode o))

      validateRequired
      validateProps
  where
    size = fromIntegral (HashMap.size o)

    validateRequired = withSchema $ \schema -> traverse_ validateReq (schema ^. required)
    validateReq name =
      when (not (HashMap.member name o)) $
        invalid ("property " ++ show name ++ " is required, but not found in " ++ show (encode o))

    validateProps = withSchema $ \schema -> do
      for_ (HashMap.toList o) $ \(k, v) ->
        case HashMap.lookup k (schema ^. properties) of
          Nothing -> check additionalProperties $ \s -> sub s $ validateWithSchema v
          Just s  -> validateWithSchemaRef s v

validateEnum :: Value -> Validation (ParamSchema t) ()
validateEnum value = do
  check enum_ $ \xs ->
    when (value `notElem` xs) $
      invalid ("expected one of " ++ show (encode xs) ++ " but got " ++ show value)

validateSchemaType :: Value -> Validation Schema ()
validateSchemaType value = withSchema $ \schema ->
  case (schema ^. type_, value) of
    (SwaggerNull,    Null)       -> valid
    (SwaggerBoolean, Bool _)     -> valid
    (SwaggerInteger, Number n)   -> sub_ paramSchema (validateInteger n)
    (SwaggerNumber,  Number n)   -> sub_ paramSchema (validateNumber n)
    (SwaggerString,  String s)   -> sub_ paramSchema (validateString s)
    (SwaggerArray,   Array xs)   -> sub_ paramSchema (validateArray xs)
    (SwaggerObject,  Object o)   -> validateObject o
    (t, _) -> invalid $ "expected JSON value of type " ++ show t ++ ": " ++ show (encode value)

validateParamSchemaType :: Value -> Validation (ParamSchema t) ()
validateParamSchemaType value = withSchema $ \schema ->
  case (schema ^. type_, value) of
    (SwaggerBoolean, Bool _)     -> valid
    (SwaggerInteger, Number n)   -> validateInteger n
    (SwaggerNumber,  Number n)   -> validateNumber n
    (SwaggerString,  String s)   -> validateString s
    (SwaggerArray,   Array xs)   -> validateArray xs
    (t, _) -> invalid $ "expected JSON value of type " ++ show t ++ ": " ++ show (encode value)

