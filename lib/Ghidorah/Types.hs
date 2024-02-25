{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ghidorah.Types where

import BasicPrelude hiding (id, isPrefixOf, lookup)
import Data.HashMap.Strict (filterWithKey)
import Data.Text (isPrefixOf, unpack)
import Data.Time (ZonedTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import GHC.Generics
import Ghidorah.Jira.CustomTypes
import Ghidorah.Jira.Types as JT
  ( ChangeDetails (..),
    Changelog (..),
    IssueTypeDetails (..),
    PageOfChangelogs (pog_histories),
    Project (..),
    Resolution (..),
    Status (..),
    UserDetails (..),
    Version (..),
  )

parseTime :: Text -> Maybe ZonedTime
parseTime = parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" . unpack

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
    created :: !ZonedTime,
    creator :: !Text,
    description :: !(Maybe Text),
    assignee :: !(Maybe Text),
    reporter :: !(Maybe Text),
    resolution :: !(Maybe Text),
    resolutiondate :: !(Maybe ZonedTime),
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
      _creator = issueObject_creator obj
  _issueTypeName <- itd_name _issueType
  _created <- parseTime $ issueObject_created obj
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
          parseTime t,
        fixversion = do
          v <- issueObject_fixVersions obj
          return $ map version_name v,
        versions = do
          v <- issueObject_versions obj
          return $ map version_name v,
        components = Nothing
      }

data Change = Change
  { change_timestamp :: !ZonedTime,
    change_author :: !Text,
    change_field :: !Text,
    change_type :: !Text,
    change_from :: !Text,
    change_fromString :: !Text,
    change_to :: !Text,
    change_toString :: !Text
  }
  deriving (Show, Generic)

getChanges :: IssueBean -> Maybe [Change]
getChanges b = do
  cs <- getChangelog b
  return $ concat $ mapMaybe toChanges cs

getChangelog :: IssueBean -> Maybe [Changelog]
getChangelog b = do
  pog <- issue_changelog b
  pog_histories pog

toChanges :: Changelog -> Maybe [Change]
toChanges c = do
  author <- changelog_author c
  user <- user_key author
  items <- changelog_items c
  timestamp <- parseTime $ changelog_created c
  return
    $ map
      ( \d ->
          Change
            { change_timestamp = timestamp,
              change_author = user,
              change_field = fromMaybe "" $ field d,
              change_type = fromMaybe "" $ fieldtype d,
              change_from = fromMaybe "" $ JT.from d,
              change_fromString = fromMaybe "" $ JT.fromString d,
              change_to = fromMaybe "" $ JT.to d,
              change_toString = fromMaybe "" $ JT.toString d
            }
      )
      items

issueType :: Text -> IssueType
issueType x = case x of
  "Story" -> Story
  "Task" -> Task
  "Bug" -> Bug
  "Epic" -> Epic
  _ -> Other

customFields :: HashMap Text Text -> HashMap Text Text
customFields = filterWithKey (\k _ -> "customfield_" `isPrefixOf` k)
