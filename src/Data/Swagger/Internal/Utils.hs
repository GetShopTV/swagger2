module Data.Swagger.Internal.Utils where

import Data.Aeson.TH
import Data.Char
import Language.Haskell.TH

jsonPrefix :: String -> Options
jsonPrefix prefix = defaultOptions
  { fieldLabelModifier      = modifier
  , constructorTagModifier  = modifier
  , sumEncoding             = ObjectWithSingleField
  }
  where
    modifier = lowerFirstLetter . drop (length prefix)

    lowerFirstLetter (c:s) = toLower c : s
    lowerFirstLetter s = s

deriveJSON' :: Name -> Q [Dec]
deriveJSON' name = deriveJSON (jsonPrefix (nameBase name)) name
