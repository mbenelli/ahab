-- |
-- Module: Ahab.Jira.Api
-- Description: Subset of Jira API
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com 
--
-- Client interface for a subset of Jira API. 
-- The most important API call is the one to "search", that 
-- allow the user to make JQL queries. 
--

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Ahab.Jira.Api where

import Ahab.Config
import Ahab.Jira.CustomTypes (IssueBean, IssueCoreObject)
import qualified Ahab.Jira.Types as JT
import BasicPrelude
import Data.Aeson
import Data.List as L
import Data.Proxy (Proxy (..))
import Data.Text as T
import GHC.Generics
import Servant.API
  ( Capture,
    Get,
    Header,
    JSON,
    Post,
    QueryParam,
    ReqBody,
    type (:<|>) (..),
    type (:>),
  )
import Servant.Client (ClientM, client)

data SearchResponse = SearchResponse
  { expand :: !(Maybe Text),
    issues :: ![IssueBean],
    maxResults :: !Int,
    names :: !(Maybe Object),
    schema :: !(Maybe Object),
    startAt :: !Int,
    total :: !Int,
    warningMessage :: !(Maybe [Text])
  }
  deriving (Show, Generic)

instance FromJSON SearchResponse

instance ToJSON SearchResponse

newtype CreateIssueRequest = CreateIssueRequest
  { fields :: IssueCoreObject
  }
  deriving (Show, Generic)

instance ToJSON CreateIssueRequest

-- API

type API =
  "rest"
    :> "api"
    :> "2"
    :> "search"
    :> QueryParam "jql" Text
    :> QueryParam "startAt" Int
    :> QueryParam "maxResults" Int
    :> QueryParam "expand" Text
    :> QueryParam "fields" Text
    :> QueryParam "fieldsByKeys" Bool
    :> Header "X-AUSERNAME" Text
    :> Header "Authorization" Text
    :> Get '[JSON] SearchResponse
    :<|> "rest"
      :> "api"
      :> "2"
      :> "field"
      :> Header "X-AUSERNAME" Text
      :> Header "Authorization" Text
      :> Get '[JSON] [JT.FieldDetails]
    :<|> "rest"
      :> "api"
      :> "2"
      :> "issuetype"
      :> Header "X-AUSERNAME" Text
      :> Header "Authorization" Text
      :> Get '[JSON] [JT.IssueTypeDetails]
    :<|> "rest"
      :> "api"
      :> "2"
      :> "issue"
      :> ReqBody '[JSON] CreateIssueRequest
      :> Header "X-AUSERNAME" Text
      :> Header "Authorization" Text
      :> Post '[JSON] IssueBean
    :<|> "rest"
      :> "api"
      :> "2"
      :> "issue"
      :> Capture "issueid" Text
      :> Header "X-AUSERNAME" Text
      :> Header "Authorization" Text
      :> Get '[JSON] IssueBean
    :<|> "rest"
      :> "api"
      :> "2"
      :> "issue"
      :> Capture "issueid" Text
      :> "changelog"
      :> Header "X-AUSERNAME" Text
      :> Header "Authorization" Text
      :> Get '[JSON] JT.PageBeanChangelog

api :: Proxy API
api = Proxy

search ::
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Text ->
  Maybe Text ->
  ClientM SearchResponse
getFields :: Maybe Text -> Maybe Text -> ClientM [JT.FieldDetails]
issueTypes :: Maybe Text -> Maybe Text -> ClientM [JT.IssueTypeDetails]
createIssueReq :: CreateIssueRequest -> Maybe Text -> Maybe Text -> ClientM IssueBean
issue :: Text -> Maybe Text -> Maybe Text -> ClientM IssueBean
changelogs :: Text -> Maybe Text -> Maybe Text -> ClientM JT.PageBeanChangelog
search :<|> getFields :<|> issueTypes :<|> createIssueReq :<|> issue :<|> changelogs = client api

query :: Text -> Text -> Config -> ClientM SearchResponse
query q f cfg =
  search
    (Just q)
    (Just 0)
    (Just 100)
    (Just "changelog")
    (Just f)
    (Just True)
    (Just $ user cfg)
    (Just $ auth cfg)

searchQuery :: Text -> Int -> [Text] -> Config -> ClientM SearchResponse
searchQuery jql start fs cfg =
  search
    (Just jql)
    (Just start)
    (Just 100)
    (Just "changelog")
    (Just $ T.concat $ L.intersperse "," fs)
    (Just True)
    (Just $ user cfg)
    (Just $ auth cfg)

fieldsQuery :: Config -> ClientM [JT.FieldDetails]
fieldsQuery cfg =
  getFields
    (Just $ user cfg)
    (Just $ auth cfg)

issueTypeQuery :: Config -> ClientM [JT.IssueTypeDetails]
issueTypeQuery cfg =
  issueTypes
    (Just $ user cfg)
    (Just $ auth cfg)

createIssue' :: CreateIssueRequest -> Config -> ClientM IssueBean
createIssue' x cfg =
  createIssueReq
    x
    (Just $ user cfg)
    (Just $ auth cfg)

issueQuery :: Text -> Config -> ClientM IssueBean
issueQuery x cfg =
  issue
    x
    (Just $ user cfg)
    (Just $ auth cfg)

changelogQuery :: Text -> Config -> ClientM JT.PageBeanChangelog
changelogQuery x cfg =
  changelogs
    x
    (Just $ user cfg)
    (Just $ auth cfg)
