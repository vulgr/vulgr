{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.Text as T
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Api.ProjectApi

type API = ProjectApi

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = getProjects -- :<|> createProject

getProjects :: Handler [ProjectSummary]
getProjects = pure $ [ProjectSummary { projectName = "test", projectDeps = Nothing, projectUrl = "test" }]

createProject :: T.Text -> Handler ProjectSummary
createProject pid = pure undefined
