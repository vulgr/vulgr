{-# LANGUAGE DeriveGeneric #-}
module Api.Common where

import Data.Aeson
import Data.Default
import qualified Data.Text as T
import GHC.Generics
import Web.HttpApiData

--------------------------------------------------------------------------------
-- Types to support paging.

newtype Limit = Limit Int deriving (Eq, Generic, Show)

instance ToJSON Limit 

instance ToHttpApiData Limit where
  toUrlPiece (Limit i)   = toUrlPiece i
  toQueryParam (Limit i) = toQueryParam i
  toHeader (Limit i)     = toHeader i

instance FromHttpApiData Limit where
  parseUrlPiece t = case (parseUrlPiece t :: Either T.Text Int) of
    Right i -> Right (Limit i)
    Left t  -> Left t

instance Default Offset where
  def = Offset 0

mkLimit n = Limit n


newtype Offset = Offset Int deriving (Eq, Generic, Show)

instance ToJSON Offset

instance ToHttpApiData Offset where
  toUrlPiece (Offset i)   = toUrlPiece i
  toQueryParam (Offset i) = toQueryParam i
  toHeader (Offset i)     = toHeader i

instance FromHttpApiData Offset where
  parseUrlPiece t = case (parseUrlPiece t :: Either T.Text Int) of
    Right i -> Right (Offset i)
    Left t  -> Left t

instance Default Limit where
  def = Limit 20

mkOffset n = Offset n


data Paged a = Paged
  { pageData   :: Maybe a
  , pageOffset :: Offset
  , pageLimit  :: Limit
  } deriving (Eq, Generic, Show)

instance ToJSON a => ToJSON (Paged a)
