{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ahab.Types where

import Ahab.Jira.CustomTypes
import qualified Ahab.Jira.Types as JT
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
import BasicPrelude hiding (id, isPrefixOf, lookup)
import Data.HashMap.Strict (filterWithKey)
import Data.Text (isPrefixOf, unpack)
import Data.Time (UTCTime, zonedTimeToUTC)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import GHC.Generics

parseTime :: Text -> Maybe UTCTime
parseTime s = do
  t <- parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" $ unpack s
  return $ zonedTimeToUTC t

data IssueType = Task | Story | Bug | Epic | Other
  deriving (Eq, Show, Generic)

newtype User = User Text
  deriving (Ord, Eq, Show, Generic)

newtype Status = Status Text
  deriving (Eq, Show, Generic)

class Issue a where
  key :: a -> Text
  issuetype :: a -> IssueType
  summary :: a -> Text
  project :: a -> Text
  status :: a -> Status
  created :: a -> UTCTime
  creator :: a -> User
  description :: a -> Maybe Text
  assignee :: a -> Maybe User
  reporter :: a -> Maybe User
  resolution :: a -> Maybe Text
  resolutiondate :: a -> Maybe UTCTime
  fixversion :: a -> Maybe [Text]
  versions :: a -> Maybe [Text]
  components :: a -> Maybe [Text]

data CoreIssue = CoreIssue
  { issue_id :: !Text,
    issue_key :: !Text,
    issue_issuetype :: !IssueType,
    issue_summary :: !Text,
    issue_project :: !Text,
    issue_status :: !Status,
    issue_created :: !UTCTime,
    issue_creator :: !User,
    issue_description :: !(Maybe Text),
    issue_assignee :: !(Maybe User),
    issue_reporter :: !(Maybe User),
    issue_resolution :: !(Maybe Text),
    issue_resolutiondate :: !(Maybe UTCTime),
    issue_fixversion :: !(Maybe [Text]),
    issue_versions :: !(Maybe [Text]),
    issue_components :: !(Maybe [Text])
  }
  deriving (Show, Generic)

instance Issue CoreIssue where
  key = issue_key
  issuetype = issue_issuetype
  summary = issue_summary
  project = issue_project
  status = issue_status
  created = issue_created
  creator = issue_creator
  description = issue_description
  assignee = issue_assignee
  reporter = issue_reporter
  resolution = issue_resolution
  resolutiondate = issue_resolutiondate
  fixversion = issue_fixversion
  versions = issue_versions
  components = issue_components

toIssue :: IssueBean -> Maybe CoreIssue
toIssue x = do
  obj <- issueBean_fields x
  let itype = issueObject_issuetype obj
      _summary = issueObject_summary obj
      _project = issueObject_project obj
      _projectName = JT.project_name _project
      _status = issueObject_status obj
      _statusName = JT.status_name _status
      _creator = issueObject_creator obj
  typename <- JT.issueTypeDetails_name itype
  _created <- parseTime $ issueObject_created obj
  creatorName <- JT.userDetails_self _creator
  return
    CoreIssue
      { issue_id = issueBean_id x,
        issue_key = issueBean_key x,
        issue_issuetype = issueType typename,
        issue_summary = _summary,
        issue_project = _projectName,
        issue_status = Status _statusName,
        issue_created = _created,
        issue_creator = User creatorName,
        issue_description = issueObject_description obj,
        issue_assignee = do
          _assignee <- issueObject_assignee obj
          _name <- JT.userDetails_self _assignee
          return $ User _name,
        issue_reporter = do
          _reporter <- issueObject_reporter obj
          _name <- JT.userDetails_self _reporter
          return $ User _name,
        issue_resolution = do
          r <- issueObject_resolution obj
          return $ JT.resolution_name r,
        issue_resolutiondate = do
          t <- issueObject_resolutiondate obj
          parseTime t,
        issue_fixversion = do
          v <- issueObject_fixVersions obj
          return $ map JT.version_name v,
        issue_versions = do
          v <- issueObject_versions obj
          return $ map JT.version_name v,
        issue_components = Nothing
      }

data Change = Change
  { change_timestamp :: !UTCTime,
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

getChangelog :: IssueBean -> Maybe [JT.Changelog]
getChangelog b = do
  pog <- issueBean_changelog b
  JT.pageOfChangelogs_histories pog

toChanges :: JT.Changelog -> Maybe [Change]
toChanges c = do
  author <- JT.changelog_author c
  user <- JT.userDetails_self author
  items <- JT.changelog_items c
  timestamp <- parseTime $ JT.changelog_created c
  return
    $ map
      ( \d ->
          Change
            { change_timestamp = timestamp,
              change_author = user,
              change_field = fromMaybe "" $ JT.changeDetails_field d,
              change_type = fromMaybe "" $ JT.changeDetails_fieldtype d,
              change_from = fromMaybe "" $ JT.changeDetails_from d,
              change_fromString = fromMaybe "" $ JT.changeDetails_fromString d,
              change_to = fromMaybe "" $ JT.changeDetails_to d,
              change_toString = fromMaybe "" $ JT.changeDetails_toString d
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
