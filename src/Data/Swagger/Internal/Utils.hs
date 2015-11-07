{-# LANGUAGE FlexibleContexts #-}
module Data.Swagger.Internal.Utils where

import Control.Arrow (first)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (Parser)
import Data.Char
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import Data.Text (Text)
import GHC.Generics
import Language.Haskell.TH

hashMapMapKeys :: (Eq k', Hashable k') => (k -> k') -> HashMap k v -> HashMap k' v
hashMapMapKeys f = HashMap.fromList . map (first f) . HashMap.toList

jsonPrefix :: String -> Options
jsonPrefix prefix = defaultOptions
  { fieldLabelModifier      = modifier
  , constructorTagModifier  = modifier
  , sumEncoding             = ObjectWithSingleField
  }
  where
    modifier = lowerFirstUppers . drop (length prefix)

    lowerFirstUppers s = map toLower x ++ y
      where (x, y) = span isUpper s

deriveToJSONDefault :: Name -> Q [Dec]
deriveToJSONDefault = deriveToJSON defaultOptions

deriveToJSON' :: Name -> Q [Dec]
deriveToJSON' name = deriveToJSON (jsonPrefix (nameBase name)) name

deriveJSONDefault :: Name -> Q [Dec]
deriveJSONDefault = deriveJSON defaultOptions

deriveJSON' :: Name -> Q [Dec]
deriveJSON' name = deriveJSON (jsonPrefix (nameBase name)) name

genericToJSONWithSub :: (Generic a, GToJSON (Rep a)) => Text -> Options -> a -> Value
genericToJSONWithSub sub opts x =
  case genericToJSON opts x of
    Object o ->
      let so = HashMap.lookupDefault (error "impossible") sub o
      in Object (HashMap.delete sub o) <+> so
    _ -> error "impossible"

genericParseJSONWithSub :: (Generic a, GFromJSON (Rep a)) => Text -> Options -> Value -> Parser a
genericParseJSONWithSub sub opts (Object o) = genericParseJSON opts json
  where
    json = Object (HashMap.insert sub (Object o) o)
genericParseJSONWithSub _ _ _ = error "impossible"

(<+>) :: Value -> Value -> Value
Object x <+> Object y = Object (x <> y)
_ <+> _ = error "impossible"
