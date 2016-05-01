{-# LANGUAGE OverloadedStrings #-}
module Graph.Project (
  createProject
  , listProjects
  ) where

import Data.Aeson (Value)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Maybe (fromJust)
import Data.Monoid (mconcat, (<>))
import Data.Pool
import qualified Data.Text as T
import qualified Database.Neo4j as Neo
import qualified Database.Neo4j.Graph as NeoGr
import qualified Database.Neo4j.Types as NeoGr
import qualified Database.Neo4j.Transactional.Cypher as TC

import Api.Project 
import Graph.Persist


-- The label for Project roots.
projectLabel :: T.Text
projectLabel = "Project"


createProject :: Pool Neo.Connection -> ProjectName -> IO (Either T.Text ())
createProject pool name = do
  eitherResult <- runTransWithPool pool $
    TC.cypher ("CREATE (project:" <> projectLabel <> "{name : {name}})") $
      M.fromList [("name", TC.newparam name)]
  case eitherResult of
    Right _                 -> pure (Right ())
    Left  (errCode, errMsg) -> pure (Left errCode)


listProjects :: Pool Neo.Connection -> Int -> Int -> IO (Either T.Text [ProjectSummary])
listProjects pool skip limit = do
  eitherResult <- runTransWithPool pool $ 
    TC.cypher ("MATCH (n:Project) RETURN (n) ORDER BY n.name SKIP " 
      <> T.pack (show skip) <> " LIMIT " <> T.pack (show limit)) $ M.empty
  case eitherResult of
    Right res       -> pure (Right $ summarizeGrs (TC.graph res))
    Left (_,err)-> pure (Left $ err)
 where
  summarizeGrs :: [NeoGr.Graph] -> [ProjectSummary]
  summarizeGrs graphs = mconcat (map (\gr -> map (\node -> summarizeProject node) $ NeoGr.getNodes gr) graphs)

summarizeProject :: NeoGr.Node -> ProjectSummary
summarizeProject node = let name = getNameAsText node in
  ProjectSummary { projectName = getNameAsText node, projectDeps = Nothing, projectUrl = "/project/" <> name }
 where
  getNameAsText node = case getNameAsPropVal node of
    NeoGr.ValueProperty val -> valToText val
    NeoGr.ArrayProperty vals -> error "This should never happen..." -- FIXME!
    
  getNameAsPropVal node = fromJust $ M.lookup "name" (Neo.getNodeProperties node)

  -- FIXME : This is pretty horrible...
  valToText val = case val of
    Neo.IntVal i    -> T.pack (show i)
    Neo.BoolVal b   -> T.pack (show b)
    Neo.TextVal t   -> t
    Neo.DoubleVal d -> T.pack (show d)
    


