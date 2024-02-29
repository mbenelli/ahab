{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module: Ahab.Types
-- Description: Types definitions
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
--
-- Type definitios for Ahab project.
-- Most important types are 'Change' and 'Issue'.
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
import Codec.Rot13
import Data.HashMap.Strict (filterWithKey)
import Data.Text (isPrefixOf, unpack)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, zonedTimeToUTC)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import GHC.Generics

data Interval = Interval !UTCTime !UTCTime
  deriving (Show, Eq)

instance Ord Interval where
  (<) a b = begin a < begin b
  (<=) a b = a == b || a < b

begin :: Interval -> UTCTime
begin (Interval b _) = b

end :: Interval -> UTCTime
end (Interval _ e) = e

duration :: Interval -> NominalDiffTime
duration (Interval b e) = diffUTCTime e b

parseTime :: Text -> Maybe UTCTime
parseTime s = do
  t <- parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" $ unpack s
  return $ zonedTimeToUTC t

newtype Status = Status Text
  deriving (Eq, Hashable, Show, Generic)

newtype IssueType = IssueType Text
  deriving (Eq, Hashable, Show, Generic)

newtype User = User Text
  deriving (Eq, Ord, Hashable, Show, Generic)

newtype Resolution = Resolution Text
  deriving (Eq, Hashable, Show, Generic)

pseudonomizeUser :: JT.UserDetails -> User
pseudonomizeUser =
  User
    . rot13
    . fromMaybe "anonymous"
    . JT.userDetails_displayName

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
  resolution :: a -> Maybe Resolution
  resolutiondate :: a -> Maybe UTCTime
  fixversion :: a -> Maybe [Text]
  versions :: a -> Maybe [Text]
  components :: a -> Maybe [Text]
  changelog :: a -> Maybe [Change]

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
    issue_resolution :: !(Maybe Resolution),
    issue_resolutiondate :: !(Maybe UTCTime),
    issue_fixversion :: !(Maybe [Text]),
    issue_versions :: !(Maybe [Text]),
    issue_components :: !(Maybe [Text]),
    issue_changelog :: !(Maybe [Change])
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
  changelog = issue_changelog

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
  return
    CoreIssue
      { issue_id = issueBean_id x,
        issue_key = issueBean_key x,
        issue_issuetype = IssueType typename,
        issue_summary = _summary,
        issue_project = _projectName,
        issue_status = Status _statusName,
        issue_created = _created,
        issue_creator = pseudonomizeUser _creator,
        issue_description = issueObject_description obj,
        issue_assignee = do
          _assignee <- issueObject_assignee obj
          return $ pseudonomizeUser _assignee,
        issue_reporter = do
          _reporter <- issueObject_reporter obj
          return $ pseudonomizeUser _reporter,
        issue_resolution = do
          r <- issueObject_resolution obj
          return $ Resolution $ JT.resolution_name r,
        issue_resolutiondate = do
          t <- issueObject_resolutiondate obj
          parseTime t,
        issue_fixversion = do
          v <- issueObject_fixVersions obj
          return $ map JT.version_name v,
        issue_versions = do
          v <- issueObject_versions obj
          return $ map JT.version_name v,
        issue_components = Nothing,
        issue_changelog = getChanges x
      }

data Change = Change
  { change_timestamp :: !UTCTime,
    change_author :: !User,
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
  items <- JT.changelog_items c
  timestamp <- parseTime $ JT.changelog_created c
  return
    $ map
      ( \d ->
          Change
            { change_timestamp = timestamp,
              change_author = pseudonomizeUser author,
              change_field = fromMaybe "" $ JT.changeDetails_field d,
              change_type = fromMaybe "" $ JT.changeDetails_fieldtype d,
              change_from = fromMaybe "" $ JT.changeDetails_from d,
              change_fromString = fromMaybe "" $ JT.changeDetails_fromString d,
              change_to = fromMaybe "" $ JT.changeDetails_to d,
              change_toString = fromMaybe "" $ JT.changeDetails_toString d
            }
      )
      items

customFields :: HashMap Text Text -> HashMap Text Text
customFields = filterWithKey (\k _ -> "customfield_" `isPrefixOf` k)
