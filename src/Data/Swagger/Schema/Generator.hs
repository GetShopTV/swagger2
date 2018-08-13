{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Swagger.Schema.Generator where

import           Control.Lens.Operators
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict.InsOrd as M
import           Data.Maybe
import           Data.Proxy
import           Data.Scientific
import           Data.Swagger
import           Data.Swagger.Declare
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Test.QuickCheck            (arbitrary)
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Property

schemaGen :: Definitions Schema -> Schema -> Gen Value
schemaGen _ schema
    | Just cases <- schema  ^. paramSchema . enum_  = elements cases
schemaGen defns schema =
    case schema ^. type_ of
      SwaggerBoolean -> Bool <$> elements [True, False]
      SwaggerNull    -> pure Null
      SwaggerNumber
        | Just min <- schema ^. minimum_
        , Just max <- schema ^. maximum_ ->
            Number . fromFloatDigits <$>
                   choose (toRealFloat min, toRealFloat max :: Double)
        | otherwise -> Number .fromFloatDigits <$> (arbitrary :: Gen Double)
      SwaggerInteger
        | Just min <- schema ^. minimum_
        , Just max <- schema ^. maximum_ ->
            Number . fromInteger <$>
                   choose (truncate min, truncate max)
        | otherwise -> Number . fromInteger <$> arbitrary
      SwaggerArray
        | Just 0 <- schema ^. maxLength -> pure $ Array V.empty
        | Just items <- schema ^. items ->
            case items of
              SwaggerItemsObject ref ->
                  let itemSchema = dereference defns ref
                  in fmap (Array . V.fromList) . listOf $ schemaGen defns itemSchema
              SwaggerItemsArray refs ->
                  let itemGens = schemaGen defns . dereference defns <$> refs
                  in fmap (Array . V.fromList) $ sequence itemGens
      SwaggerString -> String . T.pack <$> arbitrary
      SwaggerObject -> do
          let props = dereference defns <$> schema ^. properties
              reqKeys = schema ^. required
              allKeys = M.keys $ schema ^. properties
          presentKeys <- filterM (\key -> if key `elem` reqKeys
                                          then return True
                                          else arbitrary) allKeys
          let presentProps = M.filterWithKey (\k _ -> k `elem` presentKeys) props
          let gens = schemaGen defns <$> presentProps
          x <- sequence gens
          return . Object $ M.toHashMap x
  where
    dereference :: Definitions a -> Referenced a -> a
    dereference _ (Inline a)               = a
    dereference defs (Ref (Reference ref)) = fromJust $ M.lookup ref defs

genValue :: (ToSchema a) => Proxy a -> Gen Value
genValue p =
 let (defs, NamedSchema _ schema) = runDeclare (declareNamedSchema p) M.empty
 in schemaGen defs schema

validateFromJSON :: forall a . (ToSchema a, FromJSON a) => Proxy a -> Property
validateFromJSON p = forAll (genValue p) $
                       \val -> case parseEither parseJSON val of
                                 Right (_ :: a) -> succeeded
                                 Left err -> failed
                                               { reason = err
                                               }
