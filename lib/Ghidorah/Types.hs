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
    PageOfChangelogs (..),
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
  { issue_id :: !Text,
    issue_key :: !Text,
    issue_issuetype :: !IssueType,
    issue_summary :: !Text,
    issue_project :: !Text,
    issue_status :: !Text,
    issue_created :: !ZonedTime,
    issue_creator :: !Text,
    issue_description :: !(Maybe Text),
    issue_assignee :: !(Maybe Text),
    issue_reporter :: !(Maybe Text),
    issue_resolution :: !(Maybe Text),
    issue_resolutiondate :: !(Maybe ZonedTime),
    issue_fixversion :: !(Maybe [Text]),
    issue_versions :: !(Maybe [Text]),
    issue_components :: !(Maybe [Text])
  }
  deriving (Show, Generic)

toIssue :: IssueBean -> Maybe Issue
toIssue x = do
  obj <- issueBean_fields x
  let itype = issueObject_issuetype obj
      summary = issueObject_summary obj
      project = issueObject_project obj
      projectName = project_name project
      status = issueObject_status obj
      statusName = status_name status
      creator = issueObject_creator obj
  typename <- issueTypeDetails_name itype
  created <- parseTime $ issueObject_created obj
  creatorName <- userDetails_name creator
  return
    Issue
      { issue_id = issueBean_id x,
        issue_key = issueBean_key x,
        issue_issuetype = issueType typename,
        issue_summary = summary,
        issue_project = projectName,
        issue_status = statusName,
        issue_created = created,
        issue_creator = creatorName,
        issue_description = issueObject_description obj,
        issue_assignee = do
          _assignee <- issueObject_assignee obj
          userDetails_name _assignee,
        issue_reporter = do
          _reporter <- issueObject_reporter obj
          userDetails_name _reporter,
        issue_resolution = do
          r <- issueObject_resolution obj
          return $ resolution_name r,
        issue_resolutiondate = do
          t <- issueObject_resolutiondate obj
          parseTime t,
        issue_fixversion = do
          v <- issueObject_fixVersions obj
          return $ map version_name v,
        issue_versions = do
          v <- issueObject_versions obj
          return $ map version_name v,
        issue_components = Nothing
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
  pog <- issueBean_changelog b
  pageOfChangelogs_histories pog

toChanges :: Changelog -> Maybe [Change]
toChanges c = do
  author <- changelog_author c
  user <- userDetails_key author
  items <- changelog_items c
  timestamp <- parseTime $ changelog_created c
  return
    $ map
      ( \d ->
          Change
            { change_timestamp = timestamp,
              change_author = user,
              change_field = fromMaybe "" $ changeDetails_field d,
              change_type = fromMaybe "" $ changeDetails_fieldtype d,
              change_from = fromMaybe "" $ changeDetails_from d,
              change_fromString = fromMaybe "" $ changeDetails_fromString d,
              change_to = fromMaybe "" $ changeDetails_to d,
              change_toString = fromMaybe "" $ changeDetails_toString d
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
