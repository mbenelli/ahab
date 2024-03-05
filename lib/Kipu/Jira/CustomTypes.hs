{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Kipu.Jira.CustomTypes
-- Description: Custom definions of some Jira types
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
--
-- Some Jira types have many fields that are rarely used
-- and/or dependand from customizations.
-- This modules provides a core implementation of these
-- types that should work in most situation.
-- User of the library may choose to define alternatives
-- to the types defined here.
module Kipu.Jira.CustomTypes where

import Kipu.Jira.Types
import Data.Aeson
import Data.Text
import GHC.Generics

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
  { issueBean_id :: !Text,
    issueBean_key :: !Text,
    issueBean_changelog :: !(Maybe PageOfChangelogs),
    issueBean_expand :: !(Maybe Text),
    issueBean_fields :: !(Maybe IssueObject),
    issueBean_fieldsToInclude :: !(Maybe IncludedFields)
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
