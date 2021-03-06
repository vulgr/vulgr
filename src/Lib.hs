{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty   (encodePretty)
import Data.Default
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import Data.Pool
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.Neo4j as Neo
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Api.Project
import Api.Common
import Configuration
import qualified Graph.Project as Project

type Api = ProjectApi

newtype App a = App { runApp :: ReaderT (Pool Neo.Connection) (ExceptT ServantErr IO) a }
    deriving
      (
      Monad
      , Functor
      , Applicative
      , MonadReader (Pool Neo.Connection)
      , MonadIO
      , MonadError ServantErr
      )


--------------------------------------------------------------------------------
-- Servant setup.

startApp :: Configuration -> IO ()
startApp (Configuration host port user pass) =
    let hostBs = TE.encodeUtf8 host
        userBs = TE.encodeUtf8 user
        passBs = TE.encodeUtf8 pass
    in do
        pool  <- createPool (Neo.newAuthConnection hostBs port (userBs, passBs))
                            (\c -> putStrLn "Destroying... Haha not!" ) -- fixme, destroy connections!
                            6
                            10
                            10
        run 8080 (app pool)

app :: Pool Neo.Connection -> Application
app conn = serve api (readerServer conn)

api :: Proxy Api
api = Proxy

readerServer :: Pool Neo.Connection -> Server Api
readerServer pool = enter (runAppT pool) readerServerT

readerServerT :: ServerT Api App
readerServerT = listProjects :<|> createProject

-- | Transform our App type back into a Handler.
runAppT :: Pool Neo.Connection -> App :~> Handler
runAppT pool = Nat (flip runReaderT pool . runApp)


--------------------------------------------------------------------------------
-- | Api Implementation.

listProjects :: Maybe Offset -> Maybe Limit -> App (Paged [ProjectSummary])
listProjects maybeOffset maybeLimit = do
  let offset = fromMaybe def maybeOffset
  let limit  = fromMaybe def maybeLimit
  pool <- ask
  eitherResult <- liftIO (Project.listProjects pool offset limit)
  case eitherResult of
    Right ps -> pure $ (consPage ps) offset limit
    Left err -> throwError (err500 { errBody = BL.fromStrict (TE.encodeUtf8 err) })
 where
  consPage projects
    | null projects == False = Paged (Just projects)
    | otherwise = Paged (Nothing)



createProject :: ProjectName -> App ()
createProject name = do
  pool <- ask
  eitherResult <- liftIO (Project.createProject pool name)
  case eitherResult of
    Right _  -> pure ()
    Left err -> throwError (err500 { errBody = BL.fromStrict (TE.encodeUtf8 err) })

