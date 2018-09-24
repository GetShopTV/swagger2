{-# LANGUAGE TemplateHaskell #-}
-- | Like "Data.Aeson.QQ" but without interpolation.
module Data.Aeson.QQ.Simple where

import           Data.Aeson
import           Data.ByteString.Lazy.UTF8  as UTF8
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax (Lift (..))
import           Prelude                    ()
import           Prelude.Compat

aesonQQ :: QuasiQuoter
aesonQQ = QuasiQuoter
    { quoteExp  = aesonExp
    , quotePat  = const $ error "No quotePat defined for jsonQQ"
    , quoteType = const $ error "No quoteType defined for jsonQQ"
    , quoteDec  = const $ error "No quoteDec defined for jsonQQ"
    }

aesonExp :: String -> ExpQ
aesonExp txt =
  case eitherDecode $ UTF8.fromString txt of
    Left err  -> error $ "Error in aesonExp: " ++ show err
    Right val -> liftValue val

liftValue :: Value -> ExpQ
liftValue (String str) = [| String $(liftText str) |]
liftValue Null         = [| Null |]
liftValue (Number n)   = [| Number (fromRational $(return $ LitE $ RationalL (toRational n))) |]
liftValue (Bool b)     = [| Bool b |]
liftValue (Array arr)  = [| Array $ V.fromList $(ListE <$> traverse liftValue (V.toList arr)) |]
liftValue (Object obj) = [| object $jsList |]
    where
      jsList :: ExpQ
      jsList = ListE <$> traverse objs2list (HM.toList obj)

      objs2list :: (T.Text, Value) -> ExpQ
      objs2list (key, value) =  [| ($(liftText key), $(liftValue value)) |]

liftText :: T.Text -> ExpQ
liftText t = [| T.pack $(lift $ T.unpack t) |]
