{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module: Kipu.Types
-- Description: Types definitions
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
--
-- Type definitios for Kipu project.
-- Most important types are 'Change' and 'Issue'.
module Kipu.Types where

import BasicPrelude hiding (id, isPrefixOf, lookup)
import Codec.Rot13
import Data.HashMap.Strict (filterWithKey)
import Data.Text (isPrefixOf)
import Data.Time (UTCTime)
import GHC.Generics
import Kipu.Jira.CustomTypes
import qualified Kipu.Jira.Types as JT
  ( ChangeDetails (..),
    Changelog (..),
    IssueLink (..),
    IssueTypeDetails (..),
    LinkType (..),
    LinkedIssue (..),
    PageOfChangelogs (..),
    Project (..),
    Resolution (..),
    Status (..),
    UserDetails (..),
    Version (..),
  )
import Kipu.Time (parseTime)

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
  pseudononimize
    . fromMaybe "anonymous"
    . JT.userDetails_displayName

pseudononimize :: Text -> User
pseudononimize = User . rot13

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
  links :: a -> Maybe [Link]
  fixversion :: a -> Maybe [Text]
  versions :: a -> Maybe [Text]
  components :: a -> Maybe [Text]
  changelog :: a -> Maybe [Change]

data CoreIssue = CoreIssue
  { coreIssue_id :: !Text,
    coreIssue_key :: !Text,
    coreIssue_issuetype :: !IssueType,
    coreIssue_summary :: !Text,
    coreIssue_project :: !Text,
    coreIssue_status :: !Status,
    coreIssue_created :: !UTCTime,
    coreIssue_creator :: !User,
    coreIssue_description :: !(Maybe Text),
    coreIssue_assignee :: !(Maybe User),
    coreIssue_reporter :: !(Maybe User),
    coreIssue_resolution :: !(Maybe Resolution),
    coreIssue_resolutiondate :: !(Maybe UTCTime),
    coreIssue_links :: !(Maybe [Link]),
    coreIssue_fixversion :: !(Maybe [Text]),
    coreIssue_versions :: !(Maybe [Text]),
    coreIssue_components :: !(Maybe [Text]),
    coreIssue_changelog :: !(Maybe [Change])
  }
  deriving (Show, Generic)

instance Issue CoreIssue where
  key = coreIssue_key
  issuetype = coreIssue_issuetype
  summary = coreIssue_summary
  project = coreIssue_project
  status = coreIssue_status
  created = coreIssue_created
  creator = coreIssue_creator
  description = coreIssue_description
  assignee = coreIssue_assignee
  reporter = coreIssue_reporter
  resolution = coreIssue_resolution
  resolutiondate = coreIssue_resolutiondate
  fixversion = coreIssue_fixversion
  links = coreIssue_links
  versions = coreIssue_versions
  components = coreIssue_components
  changelog = coreIssue_changelog

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
      { coreIssue_id = issueBean_id x,
        coreIssue_key = issueBean_key x,
        coreIssue_issuetype = IssueType typename,
        coreIssue_summary = _summary,
        coreIssue_project = _projectName,
        coreIssue_status = Status _statusName,
        coreIssue_created = _created,
        coreIssue_creator = pseudonomizeUser _creator,
        coreIssue_description = issueObject_description obj,
        coreIssue_assignee =
          issueObject_assignee obj
            >>= Just
            . pseudonomizeUser,
        coreIssue_reporter =
          issueObject_reporter obj
            >>= Just
            . pseudonomizeUser,
        coreIssue_resolution =
          issueObject_resolution obj
            >>= Just
            . Resolution
            . JT.resolution_name,
        coreIssue_resolutiondate =
          issueObject_resolutiondate obj
            >>= parseTime,
        coreIssue_links =
          issueObject_issuelinks obj
            >>= \ls -> Just $ toLinks ls,
        coreIssue_fixversion =
          issueObject_fixVersions obj
            >>= \vs -> Just $ map JT.version_name vs,
        coreIssue_versions =
          issueObject_versions obj
            >>= \vs -> Just $ map JT.version_name vs,
        coreIssue_components = Nothing,
        coreIssue_changelog = getChanges x
      }

-- History
--

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

-- Links
--

newtype LinkType = LinkType Text
  deriving (Eq, Hashable, Show, Generic)

data Link = Link
  { link_type :: !LinkType,
    link_targetKey :: !Text
  }
  deriving (Eq, Show, Generic)

-- | Convert Jira issue links into Links
toLinks :: [JT.IssueLink] -> [Link]
toLinks =
  foldl'
    ( \r x ->
        let t = JT.issueLink_type x
            inward = LinkType $ JT.linkType_inward t
            outward = LinkType $ JT.linkType_outward t
         in case JT.issueLink_outwardIssue x of
              Just o -> (Link outward (JT.linkedIssue_key o)) : r
              Nothing -> case JT.issueLink_inwardIssue x of
                Just i -> (Link inward (JT.linkedIssue_key i)) : r
                Nothing -> r
    )
    []

-- | Convert Jira issue linkes into links, partitioning inward and outwards
inOutLinks :: [JT.IssueLink] -> ([Link], [Link])
inOutLinks =
  foldl'
    ( \r x ->
        let t = JT.issueLink_type x
            inward = LinkType $ JT.linkType_inward t
            outward = LinkType $ JT.linkType_outward t
         in case JT.issueLink_outwardIssue x of
              Just o -> (fst r, (Link outward (JT.linkedIssue_key o)) : (snd r))
              Nothing -> case JT.issueLink_inwardIssue x of
                Just i -> ((Link inward (JT.linkedIssue_key i)) : (fst r), (snd r))
                Nothing -> r
    )
    ([], [])

customFields :: HashMap Text Text -> HashMap Text Text
customFields = filterWithKey (\k _ -> "customfield_" `isPrefixOf` k)
