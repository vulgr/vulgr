{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Api.Dependency where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Servant

import Api.Project 

data DependencySummary = DependencySummary 
  { dependencyName    :: T.Text
  , dependencyVersion :: Maybe T.Text
  , dependencyDesc    :: Maybe T.Text
  , dependencyDeps    :: Maybe T.Text -- FIXME : path to dependency list.
  , dependencyUrl     :: T.Text 
  } deriving (Eq, Show)

type Dependency = T.Text -- FIXME : Should be npm/gradle deps
type DependencyRoot = ProjectRoot :> ProjectCapture :> "dependency" 

type DependencyApi =
  --  Upload dependency data into the project.
  DependencyRoot :> (
    ReqBody '[JSON] Dependency :> Post '[JSON] ()
  
    :<|> Capture "dependencyId" :> Get '[JSON] ()
  )
