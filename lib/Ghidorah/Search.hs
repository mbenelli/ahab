{-# LANGUAGE DeriveGeneric #-}

module Ghidorah.Search where

import Data.Aeson (FromJSON)
import Data.Text
import GHC.Generics (Generic)

import Ghidorah.Field (Field)

data IncludedFields = IncludedFields
  { actuallyIncluded :: ![Text]
  , excluded :: ![Text]
  , includede :: ![Text]
  } deriving (Show, Generic)

instance FromJSON IncludedFields


data ChangeDetails = ChangeDetails
  { field :: !String
  , fieldId :: !String
  , fieldtype :: !String
  , from :: !String
  , fromString :: !String
  , to :: !String
  , toString :: !String
  } deriving (Show, Generic)

instance FromJSON ChangeDetails

data HistoryMetadataPartecipant = HistoryMetadataPartecipant
  { avatarUrl :: !(Maybe Text)
  , displayName :: !(Maybe Text)
  , displayNameKey :: !(Maybe Text)
  , id''' :: !(Maybe Text)
  , type'' :: !(Maybe Text)
  , url' :: !(Maybe Text)
  } deriving (Show, Generic)

instance FromJSON HistoryMetadataPartecipant

data HistoryMetadata = HistoryMetadata
  { activityDescription :: !(Maybe Text)
  , activityDescriptionKey :: !(Maybe Text)
  , actor :: !(Maybe HistoryMetadataPartecipant)
  , cause :: !(Maybe HistoryMetadataPartecipant)
  , description :: !(Maybe Text)
  , descriptionKey :: !(Maybe Text)
  , emailDescription :: !(Maybe Text)
  , emailDescriptionKey :: !(Maybe Text)
  , generator :: !(Maybe HistoryMetadataPartecipant)
  } deriving (Show, Generic)

instance FromJSON HistoryMetadata

data AvatarUrlsBean = AvatarUrlsBean
  { avatar16x16 :: !(Maybe Text)
  , avatar24x24 :: !(Maybe Text)
  , avatar32x32 :: !(Maybe Text)
  , avatar48x48 :: !(Maybe Text)
  } deriving (Show, Generic)

instance FromJSON AvatarUrlsBean

data UserDetail = UserDetail
  { accountId :: !(Maybe Text)
  , accountType :: !(Maybe Text)
  , active :: !(Maybe Bool)
  , avatarUrls :: !(Maybe AvatarUrlsBean)
  , displayName' :: !(Maybe Text)
  , emailAddress :: !(Maybe Text)
  , key :: !(Maybe Text)
  , name :: !(Maybe Text)
  , self :: !(Maybe Text)
  , timeZone :: !(Maybe Text)
  } deriving (Show, Generic)

instance FromJSON UserDetail

data Changelog = Changelog
  { author :: !(Maybe UserDetail)
  , created :: !Text
  , historyMetadata :: !(Maybe HistoryMetadata)
  , id'' :: !(Maybe Text)
  , items :: !(Maybe [ChangeDetails])
  } deriving (Show, Generic)

instance FromJSON Changelog

data PageOfChangelogs = PageOfChangelogs
  { histories :: !(Maybe [Changelog])
  , maxResults' :: !(Maybe Int)
  , startAt' :: !(Maybe Int)
  , total' :: !(Maybe Int)
  } deriving (Show, Generic)

instance FromJSON PageOfChangelogs


data IssueBean = IssueBean
  {
  changelog' :: !(Maybe PageOfChangelogs)
  --, editmeta :: !Text -- IssueUpdateMetadata
  , expand' :: !(Maybe Text)
  , fields :: !(Maybe [Field])
  , fieldsToInclude :: !(Maybe IncludedFields)
  --,
  , id :: !Text
  , key' :: !Text
  --, names_ :: !Text -- JSON
  --, operation :: !Text -- Operations
  --, properties :: !Text -- JSON
  --, renderedFields :: !Text -- JSON
  --, schema_ :: !Text -- JSON
  --, self :: !Text
  --, transitions :: !Text -- [IssueTransition]
  --, versionedRepresentation :: !Text -- JSON
  } deriving (Show, Generic)

instance FromJSON IssueBean

data SearchResponse = SearchResponse
  { expand :: !Text
  , issues :: ![IssueBean]
  , maxResults :: !Int
--  , names :: !Text  -- JSON
--  , schema :: !Text -- JSON
  , startAt :: !Integer
  , total :: !Integer
  , warningMessage :: !(Maybe [Text])
  } deriving (Show, Generic)

instance FromJSON SearchResponse



