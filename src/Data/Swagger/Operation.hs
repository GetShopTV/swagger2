{-# LANGUAGE RankNTypes #-}
module Data.Swagger.Operation where

import Control.Lens
import Data.Data.Lens

import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (mapMaybe)
import Data.Monoid
import Data.Swagger.Declare
import Data.Swagger.Internal
import Data.Swagger.Lens
import Data.Swagger.Schema

-- $setup
-- >>> import Data.Aeson
-- >>> import Data.Proxy
-- >>> import Data.Time

-- | @'operationsOf' sub@ will traverse only those operations
-- that are present in @sub@. Note that @'Operation'@ is determined
-- by both path and method.
--
-- >>> let ok = (mempty :: Operation) & at 200 ?~ "OK"
-- >>> let api = (mempty :: Swagger) & paths .~ [("/user", mempty & get ?~ ok & post ?~ ok)]
-- >>> let sub = (mempty :: Swagger) & paths .~ [("/user", mempty & get ?~ mempty)]
-- >>> encode api
-- "{\"swagger\":\"2.0\",\"info\":{\"version\":\"\",\"title\":\"\"},\"paths\":{\"/user\":{\"post\":{\"responses\":{\"200\":{\"description\":\"OK\"}}},\"get\":{\"responses\":{\"200\":{\"description\":\"OK\"}}}}}}"
-- >>> encode $ api & operationsOf sub . at 404 ?~ "Not found"
-- "{\"swagger\":\"2.0\",\"info\":{\"version\":\"\",\"title\":\"\"},\"paths\":{\"/user\":{\"post\":{\"responses\":{\"200\":{\"description\":\"OK\"}}},\"get\":{\"responses\":{\"404\":{\"description\":\"Not found\"},\"200\":{\"description\":\"OK\"}}}}}}"
operationsOf :: Swagger -> Traversal' Swagger Operation
operationsOf sub = paths.itraversed.withIndex.subops
  where
    -- | Traverse operations that correspond to paths and methods of the sub API.
    subops :: Traversal' (FilePath, PathItem) Operation
    subops f (path, item) = case HashMap.lookup path (sub ^. paths) of
      Just subitem -> (,) path <$> methodsOf subitem f item
      Nothing      -> pure (path, item)

    -- | Traverse operations that exist in a given @'PathItem'@
    -- This is used to traverse only the operations that exist in sub API.
    methodsOf :: PathItem -> Traversal' PathItem Operation
    methodsOf pathItem = partsOf template . itraversed . indices (`elem` ns) . _Just
      where
        ops = pathItem ^.. template :: [Maybe Operation]
        ns = mapMaybe (fmap fst . sequenceA) $ zip [0..] ops

-- | Apply tags to a part of Swagger spec and update the global
-- list of tags.
applyTagsFor :: Swagger -> [Tag] -> Swagger -> Swagger
applyTagsFor sub ts swag = swag
  & operationsOf sub . tags %~ (map _tagName ts ++)
  & tags %~ (ts ++)

-- | Construct a response with @'Schema'@ while declaring all
-- necessary schema definitions.
--
-- >>> encode $ runDeclare (declareResponse (Proxy :: Proxy Day)) mempty
-- "[{\"Day\":{\"format\":\"date\",\"type\":\"string\"}},{\"schema\":{\"$ref\":\"#/definitions/Day\"},\"description\":\"\"}]"
declareResponse :: ToSchema a => proxy a -> Declare (Definitions Schema) Response
declareResponse proxy = do
  s <- declareSchemaRef proxy
  return (mempty & schema ?~ s)

-- | Set response for all operations of a subspec.
-- This will also update global schema definitions.
--
-- >>> let api = (mempty :: Swagger) & paths .~ [("/user", mempty & get ?~ mempty)]
-- >>> let res = declareResponse (Proxy :: Proxy Day)
-- >>> encode $ api & setResponseFor api 200 res
-- "{\"swagger\":\"2.0\",\"info\":{\"version\":\"\",\"title\":\"\"},\"definitions\":{\"Day\":{\"format\":\"date\",\"type\":\"string\"}},\"paths\":{\"/user\":{\"get\":{\"responses\":{\"200\":{\"schema\":{\"$ref\":\"#/definitions/Day\"},\"description\":\"\"}}}}}}"
setResponseFor :: Swagger -> HttpStatusCode -> Declare (Definitions Schema) Response -> Swagger -> Swagger
setResponseFor sub code dres swag = swag
  & definitions %~ (<> defs)
  & operationsOf sub . at code ?~ Inline res
  where
    (defs, res) = runDeclare dres mempty

