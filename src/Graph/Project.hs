{-# LANGUAGE OverloadedStrings #-}
module Graph.Project where

import qualified Data.HashMap.Strict as M
import Data.Monoid ((<>))
import Data.Pool
import qualified Data.Text as T
import qualified Database.Neo4j as Neo
import qualified Database.Neo4j.Transactional.Cypher as TC

import Api.Project 
import Graph.Persist

-- The label for Project roots.
projectLabel :: T.Text
projectLabel = "Project"


createProject :: Pool Neo.Connection -> ProjectName -> IO (Either T.Text ())
createProject pool name = do
  eitherResult <- runTransWithPool pool $
    TC.cypher ("CREATE (project:" <> projectLabel <> "{name : {name})") $
      M.fromList [("name", TC.newparam name)]
  case eitherResult of
    Right _                 -> pure (Right ())
    Left  (errCode, errMsg) -> pure (Left errCode)
