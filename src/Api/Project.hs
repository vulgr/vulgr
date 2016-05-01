{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Api.Project where

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

type ProjectName = T.Text
type ProjectRoot = "project" 
type ProjectCapture = Capture "projectName" ProjectName

type ProjectApi =
  -- List all projects and summarise.
  ProjectRoot :> (
    Get '[JSON] [ProjectSummary]

    :<|> ProjectCapture :> Post '[JSON] () 
  )
 

