{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics

import Data.Swagger
import Data.Swagger.Declare
import Data.Swagger.Lens

type Username = Text

data UserSummary = UserSummary
  { summaryUsername :: Username
  , summaryUserid   :: Int
  } deriving (Generic, ToSchema)

type Group = Text

data UserDetailed = UserDetailed
  { username :: Username
  , userid   :: Int
  , groups   :: [Group]
  } deriving (Generic, ToSchema)

newtype Package = Package { packageName :: Text }
  deriving (Generic, ToSchema)

hackageSwagger :: Swagger
hackageSwagger = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareHackageSwagger mempty

declareHackageSwagger :: Declare Definitions Swagger
declareHackageSwagger = do
  let usernameParamSchema = toParamSchema (Proxy :: Proxy Username)
  userSummarySchemaRef  <- declareSchemaRef (Proxy :: Proxy UserSummary)
  userDetailedSchemaRef <- declareSchemaRef (Proxy :: Proxy UserDetailed)
  packagesSchemaRef     <- declareSchemaRef (Proxy :: Proxy [Package])
  return $ mempty
    & paths.pathsMap .~
        [ ("/users", mempty & pathItemGet ?~ (mempty
            & operationProduces ?~ MimeList ["application/json"]
            & operationResponses .~ (mempty
                & responsesResponses . at 200 ?~ Inline (mempty & responseSchema ?~ userSummarySchemaRef))))
        , ("/user/{username}", mempty & pathItemGet ?~ (mempty
            & operationProduces ?~ MimeList ["application/json"]
            & operationParameters .~ [ Inline $ mempty
                & paramName .~ "username"
                & paramRequired ?~ True
                & paramSchema .~ ParamOther (mempty
                    & paramOtherSchemaIn .~ ParamPath
                    & paramOtherSchemaParamSchema .~ usernameParamSchema) ]
            & operationResponses .~ (mempty
                & responsesResponses . at 200 ?~ Inline (mempty & responseSchema ?~ userDetailedSchemaRef))))
        , ("/packages", mempty & pathItemGet ?~ (mempty
            & operationProduces ?~ MimeList ["application/json"]
            & operationResponses .~ (mempty
                & responsesResponses . at 200 ?~ Inline (mempty & responseSchema ?~ packagesSchemaRef))))
        ]

main :: IO ()
main = putStrLn . read . show . encode $ hackageSwagger

