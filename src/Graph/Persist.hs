{-# LANGUAGE OverloadedStrings #-}
module Graph.Persist where

import qualified Data.HashMap.Strict as M
import Data.Pool
import qualified Data.Text as T
import qualified Database.Neo4j as Neo
import qualified Database.Neo4j.Transactional.Cypher as TC

import Graph.UniqueNodeGraph

-- | Bootstrap graph constraints and data.
-- FIXME: Should not be in this module....
bootstrap :: Pool Neo.Connection -> IO (Either TC.TransError TC.Result)
bootstrap pool = runTransWithPool pool $ do
  TC.cypher "CREATE CONSTRAINT ON (project:Project) ASSERT project.name IS UNIQUE" $ 
    M.empty
 

runTransWithPool :: Pool Neo.Connection -> TC.Transaction a -> IO (Either TC.TransError a)
runTransWithPool pool action = withResource pool $ \conn -> do
  n4jTransaction conn action


n4jTransaction :: Neo.Connection -> TC.Transaction a ->  IO (Either TC.TransError a)
n4jTransaction conn action = flip Neo.runNeo4j conn $
  TC.runTransaction action
