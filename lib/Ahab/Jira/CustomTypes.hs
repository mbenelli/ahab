{-# LANGUAGE DeriveGeneric #-}

module Ahab.Jira.CustomTypes where

import Data.Aeson
import Data.Text
import GHC.Generics
import Ahab.Jira.Types

data IssueObject = IssueObject
  { issueObject_project :: !Project,
    issueObject_issuetype :: !IssueTypeDetails,
    issueObject_summary :: !Text,
    issueObject_status :: !Status,
    issueObject_created :: !Text,
    issueObject_creator :: !UserDetails,
    issueObject_priority :: !(Maybe Priority),
    issueObject_description :: !(Maybe Text),
    issueObject_assignee :: !(Maybe UserDetails),
    issueObject_reporter :: !(Maybe UserDetails),
    issueObject_fixVersions :: !(Maybe [Version]),
    issueObject_versions :: !(Maybe [Version]),
    issueObject_components :: !(Maybe [Component]),
    issueObject_issuelink :: !(Maybe IssueLink),
    issueObject_resolution :: !(Maybe Resolution),
    issueObject_resolutiondate :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON IssueObject where parseJSON = genericParseJSON options

instance ToJSON IssueObject where toJSON = genericToJSON options

data IssueBean = IssueBean
  { issueBean_changelog :: !(Maybe PageOfChangelogs),
    issueBean_expand :: !(Maybe Text),
    issueBean_fields :: !(Maybe IssueObject),
    issueBean_fieldsToInclude :: !(Maybe IncludedFields),
    issueBean_id :: !Text,
    issueBean_key :: !Text
  }
  deriving (Show, Generic)

instance FromJSON IssueBean where
  parseJSON = genericParseJSON options

instance ToJSON IssueBean where
  toJSON = genericToJSON options

data IssueCoreObject = IssueCoreObject
  { issueCoreObject_summary :: !Text,
    issueCoreObject_description :: !(Maybe Text),
    issueCoreObject_issuetype :: !IssueType,
    issueCoreObject_project :: !Project
  }
  deriving (Show, Generic)

instance ToJSON IssueCoreObject where
  toEncoding = genericToEncoding options
