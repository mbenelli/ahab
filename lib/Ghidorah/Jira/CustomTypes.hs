{-# LANGUAGE DeriveGeneric #-}

module Ghidorah.Jira.CustomTypes where

import Data.Aeson
import Data.Text
import GHC.Generics
import Ghidorah.Jira.Types

data IssueObject = IssueObject
  { issueObject_project :: !Project,
    issueObject_issuetype :: !IssueTypeDetails,
    issueObject_summary :: !Text,
    issueObject_status :: !Status,
    issueObject_created :: !Text,
    issueObject_creator :: !UserDetails,
    issueObject_priority :: !Priority,
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

data IssueBean = IssueBean
  { issue_changelog :: !(Maybe PageOfChangelogs),
    issue_editmeta :: !(Maybe Text), -- IssueUpdateMetadata
    issue_expand :: !(Maybe Text),
    issue_fields :: !(Maybe IssueObject),
    issue_fieldsToInclude :: !(Maybe IncludedFields),
    --
    issue_id :: !Text,
    issue_key :: !Text
    -- , issue_names :: !(Maybe Object)
    -- , issue_operation :: !Text -- Operations
    -- , issue_properties :: !(Maybe Object)
    -- , issue_renderedFields :: !(Maybe Object)
    -- , issue_schema :: !(Maybe Object)
    -- , issue_self :: !Text
    -- , issue_transitions :: !Text -- [IssueTransition]
    -- , issue_versionedRepresentation :: !(Maybe Object)
  }
  deriving (Show, Generic)

instance FromJSON IssueBean where
  parseJSON = genericParseJSON options

data IssueCoreObject = IssueCoreObject
  { i_summary :: !Text,
    i_description :: !(Maybe Text),
    i_issuetype :: !IssueType,
    i_project :: !Project
  }
  deriving (Show, Generic)

instance ToJSON IssueCoreObject where
  toEncoding = genericToEncoding options
