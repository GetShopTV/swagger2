module Data.Swagger.Internal.Utils where

import Data.Aeson.TH
import Data.Char
import Language.Haskell.TH

jsonOptions :: String -> Options
jsonOptions prefix = defaultOptions
  { fieldLabelModifier      = modifier
  , constructorTagModifier  = modifier
  , sumEncoding             = ObjectWithSingleField
  }
  where
    modifier = lowerFirstLetter . drop (length prefix)

    lowerFirstLetter (c:s) = toLower c : s
    lowerFirstLetter s = s

deriveJSON' :: Name -> Q [Dec]
deriveJSON' name = deriveJSON (jsonOptions (nameBase name)) name
