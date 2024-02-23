{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ghidorah.Types where

import BasicPrelude hiding (id, isPrefixOf, lookup)
import Data.HashMap.Strict (filterWithKey, fromList, lookup, (!))
import Data.Text (isPrefixOf, splitOn, unpack)
import Data.Time (UTCTime)
import Data.Time.Format (parseTimeM)
import Data.Time.Format.ISO8601
import Data.Time.LocalTime (utc)
import GHC.Generics
import Ghidorah.Jira.CustomTypes
import Ghidorah.Jira.CustomTypes (IssueBean (..))
import Ghidorah.Jira.Types
  ( Component,
    IssueTypeDetails (..),
    Project (..),
    Resolution (..),
    Status (..),
    UserDetails (..),
    Version (..),
  )

data IssueType = Task | Story | Bug | Epic | Other
  deriving (Show, Generic)

data Status = New | ToDo | InProgress | Closed | Blocked
  deriving (Show, Generic)

data Issue = Issue
  { id :: !Text,
    key :: !Text,
    issuetype :: !IssueType,
    summary :: !Text,
    project :: !Text,
    status :: !Text,
    created :: !(Maybe UTCTime),
    creator :: !Text,
    description :: !(Maybe Text),
    assignee :: !(Maybe Text),
    reporter :: !(Maybe Text),
    resolution :: !(Maybe Text),
    resolutiondate :: !(Maybe UTCTime),
    fixversion :: !(Maybe [Text]),
    versions :: !(Maybe [Text]),
    components :: !(Maybe [Text])
  }
  deriving (Show, Generic)

toIssue :: IssueBean -> Maybe Issue
toIssue x = do
  obj <- issue_fields x
  let _issueType = issueObject_issuetype obj
      _summary = issueObject_summary obj
      _project = issueObject_project obj
      _projectName = project_name _project
      _status = issueObject_status obj
      _statusName = status_name _status
      _created = iso8601ParseM $ unpack $ issueObject_created obj
      _creator = issueObject_creator obj
  _issueTypeName <- itd_name _issueType
  _creatorName <- user_name _creator
  return
    Issue
      { id = issue_id x,
        key = issue_key x,
        issuetype = issueType _issueTypeName,
        summary = _summary,
        project = _projectName,
        status = _statusName,
        created = _created,
        creator = _creatorName,
        description = issueObject_description obj,
        assignee = do
          _assignee <- issueObject_assignee obj
          user_name _assignee,
        reporter = do
          _reporter <- issueObject_reporter obj
          user_name _reporter,
        resolution = do
          r <- issueObject_resolution obj
          return $ resolution_name r,
        resolutiondate = do
          t <- issueObject_resolutiondate obj
          iso8601ParseM $ unpack t,
        fixversion = do
          v <- issueObject_fixVersions obj
          return $ map version_name v,
        versions = do
          v <- issueObject_versions obj
          return $ map version_name v,
        components = Nothing
      }

issueType :: Text -> IssueType
issueType x = case x of
  "Story" -> Story
  "Task" -> Task
  "Bug" -> Bug
  "Epic" -> Epic
  _ -> Other

customFields :: HashMap Text Text -> HashMap Text Text
customFields = filterWithKey (\k _ -> "customfield_" `isPrefixOf` k)
