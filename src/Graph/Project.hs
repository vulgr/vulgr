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
import Api.Common
import Graph.Persist

import Debug.Trace

--------------------------------------------------------------------------------
-- The label for Project roots.
projectLabel :: T.Text
projectLabel = "Project"


--------------------------------------------------------------------------------
-- | Create a project.
createProject :: Pool Neo.Connection -> ProjectName -> IO (Either T.Text ())
createProject pool name = do
  eitherResult <- runTransWithPool pool $
    TC.cypher ("CREATE (project:" <> projectLabel <> "{name : {name}})") $
      M.fromList [("name", TC.newparam name)]
  case eitherResult of
    Right _                 -> pure (Right ())
    Left  (errCode, errMsg) -> pure (Left errCode)


--------------------------------------------------------------------------------
-- | List all projects. Control the amount of projects shown with offset and
-- limit.
listProjects :: Pool Neo.Connection
             -> Offset
             -> Limit
             -> IO (Either T.Text [ProjectSummary])
listProjects pool offset limit = do
  eitherResult <- runTransWithPool pool $
    TC.cypher (query offset limit) M.empty
  case eitherResult of
    Right res    -> pure (Right $ summarizeGrs (TC.graph res))
    Left (_,err) -> pure (Left $ err)
 where
  summarizeGrs :: [NeoGr.Graph] -> [ProjectSummary]
  summarizeGrs graphs = mconcat (map mapNodes graphs)

  mapNodes :: NeoGr.Graph -> [ProjectSummary]
  mapNodes gr = map summarizeProject (NeoGr.getNodes gr)

  query :: Offset -> Limit -> T.Text
  query (Offset offset) (Limit limit) =
    "MATCH (n:Project) RETURN (n) ORDER BY n.name SKIP "
      <> T.pack (show offset)
      <> " LIMIT "
      <> T.pack (show limit)


--------------------------------------------------------------------------------
-- | Convert a node representing a project into a summarized representation of
-- the project.
summarizeProject :: NeoGr.Node -> ProjectSummary
summarizeProject node =
  let name = getNameAsText node
  in  ProjectSummary
      { projectName = getNameAsText node
      , projectDeps = Nothing
      , projectUrl = "/project/" <> name
      }
 where
  getNameAsText node = case getNameAsPropVal node of
    NeoGr.ValueProperty val -> n4jValToText val
    NeoGr.ArrayProperty vals -> error "This should never happen..." -- FIXME!

  getNameAsPropVal node = fromJust $ M.lookup "name" (Neo.getNodeProperties node)


n4jValToText :: Neo.Val -> T.Text
n4jValToText val = case val of
    Neo.IntVal i    -> T.pack (show i)
    Neo.BoolVal b   -> T.pack (show b)
    Neo.TextVal t   -> t
    Neo.DoubleVal d -> T.pack (show d)



