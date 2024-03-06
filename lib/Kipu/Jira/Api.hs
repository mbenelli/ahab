{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Kipu.Jira.Api
-- Description: Subset of Jira API
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
--
-- Client interface for a subset of Jira API.
-- The most important API call is the one to "search", that
-- allow the user to make JQL queries.
module Kipu.Jira.Api where

import BasicPrelude
import Data.Aeson
import Data.List as L
import Data.Proxy (Proxy (..))
import Data.Text as T
import GHC.Generics
import Kipu.Config
import Kipu.Jira.CustomTypes (IssueBean, IssueCoreObject)
import qualified Kipu.Jira.InsightTypes as JI
import qualified Kipu.Jira.Types as JT
import Servant.API
  ( Capture,
    Get,
    Header,
    JSON,
    PlainText,
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

type AssetSearchResponse = Object

data AssetSearchResponse' = AssetSearchResponse'
  { asset_id :: !(Maybe Text),
    asset_title :: !(Maybe Text),
    asset_type :: !(Maybe Text),
    asset_properties :: !(Maybe Object),
    asset_definitions :: !(Maybe Object),
    asset_additionalProperties :: !(Maybe Bool),
    asset_required :: !(Maybe Object)
  }
  deriving (Show, Generic)

instance FromJSON AssetSearchResponse' where
  parseJSON = genericParseJSON JT.options

data InsightSearchResponse = InsightSearchResponse
  { insightSearchResponse_objectEntries :: ![JI.ObjectEntry],
    insightSearchResponse_objectTypeAttributes :: ![Maybe Object],
    insightSearchResponse_iql :: !(Maybe Text),
    insightSearchResponse_qlQuery :: !(Maybe Text),
    insightSearchResponse_startIndex :: !Int,
    insightSearchResponse_toIndex :: !Int,
    insightSearchResponse_totalFilterCount :: !(Maybe Int)
  }
  deriving (Show, Generic)

instance FromJSON InsightSearchResponse where
  parseJSON = genericParseJSON JT.options

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
    :<|> "rest"
      :> "servicedeskapi"
      :> "assets"
      :> "workspace"
      :> Header "X-USERNAME" Text
      :> Header "Authorization" Text
      :> Get '[PlainText] Text
    :<|> "rest"
      :> "insight"
      :> "1.0"
      :> "iql"
      :> "objects"
      :> QueryParam "qlQuery" Text
      :> QueryParam "page" Int
      :> Header "X-USERNAME" Text
      :> Header "Authorization" Text
      :> Get '[JSON] InsightSearchResponse -- AssetSearchResponse

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
workspaceid :: Maybe Text -> Maybe Text -> ClientM Text
assetSearch :: Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> ClientM InsightSearchResponse
search
  :<|> getFields
  :<|> issueTypes
  :<|> createIssueReq
  :<|> issue
  :<|> changelogs
  :<|> workspaceid
  :<|> assetSearch = client api

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

workspaceidQuery :: Config -> ClientM Text
workspaceidQuery cfg =
  workspaceid
    (Just $ user cfg)
    (Just $ auth cfg)

assetQuery :: Text -> Config -> ClientM InsightSearchResponse
assetQuery x cfg =
  assetSearch
    (Just x)
    Nothing
    (Just $ user cfg)
    (Just $ auth cfg)
