{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Api.ProjectApi where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Servant


data ProjectSummary = ProjectSummary
  { projectName :: T.Text
  , projectDeps :: Maybe T.Text -- FIXME : path to dependency list.
  , projectUrl  :: T.Text -- FIXME : path not text.
  } deriving (Eq, Generic, Show)

instance ToJSON ProjectSummary

data DependencySummary = DependencySummary 
  { dependencyName    :: T.Text
  , dependencyVersion :: Maybe T.Text
  , dependencyDesc    :: Maybe T.Text
  , dependencyDeps    :: Maybe T.Text -- FIXME : path to dependency list.
  , dependencyUrl     :: T.Text 
  } deriving (Eq, Show)

type Dependency = T.Text -- FIXME : Should be npm/gradle deps
type ProjectId = T.Text
type ProjectRoot = "project" :> Capture "projectId" ProjectId
type DependencyRoot = ProjectRoot :> "dependency" 

type ProjectApi =
  "project" :> ( 
    -- List all projects and summarise.
    Get '[JSON] [ProjectSummary]

    :<|> Capture "projectId" ProjectId :> ( 

      -- Create a project.      
      Post '[JSON] () 
    )
  )
 
type DependencyApi =
  --  Upload dependency data into the project.
  DependencyRoot :> ReqBody '[JSON] Dependency :> Post '[JSON] ()
  
  -- 
  :<|> DependencyRoot :> Capture "dependencyId" 
