{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ghidorah.Types where

import BasicPrelude hiding (id, isPrefixOf, lookup)
import Data.HashMap.Strict (filterWithKey, fromList, lookup, (!))
import Data.Text (isPrefixOf)
import GHC.Generics
import Ghidorah.Jira.CustomTypes (IssueBean (..))
import Ghidorah.Jira.Types (Field (..))

data IssueType = Task | Story | Bug | Epic | Other
  deriving (Show, Generic)

data Issue = Issue
  { id :: !Text,
    key :: !Text,
    issuetype :: !IssueType,
    summary :: !Text,
    project :: !Text,
    status :: !Text,
    created :: !Text,
    creator :: !Text,
    description :: !(Maybe Text),
    assignee :: !(Maybe Text),
    reporter :: !(Maybe Text),
    resolution :: !(Maybe Text),
    resolutiondate :: !(Maybe Text),
    fixversion :: !(Maybe Text),
    versions :: !(Maybe Text),
    components :: !(Maybe Text),
    customfields :: !(HashMap Text Text)
  }
  deriving (Show, Generic)

toIssue :: IssueBean -> Maybe Issue
toIssue x = case issue_fields x of
  Nothing -> Nothing
  Just fs ->
    Just
      Issue
        { id = issue_id x,
          key = issue_key x,
          issuetype = issueType (fields ! "issuetype"),
          summary = fields ! "summary",
          project = fields ! "project",
          status = fields ! "status",
          created = fields ! "created",
          creator = fields ! "creator",
          description = lookup "description" fields,
          assignee = lookup "assignee" fields,
          reporter = lookup "reporter" fields,
          resolution = lookup "resolution" fields,
          resolutiondate = lookup "resolutiondate" fields,
          fixversion = lookup "fixversion" fields,
          versions = lookup "versions" fields,
          components = lookup "components" fields,
          customfields = customFields fields
        }
    where
      fields = fieldToMap fs

fieldToMap :: [Field] -> HashMap Text Text
fieldToMap fs =
  fromList
    $ mapMaybe
      ( \x -> case fieldName x of
          Just n -> Just (fieldId' x, n)
          Nothing -> Nothing
      )
      fs

issueType :: Text -> IssueType
issueType x = case x of
  "Story" -> Story
  "Task" -> Task
  "Bug" -> Bug
  "Epic" -> Epic
  _ -> Other

customFields :: (HashMap Text Text) -> (HashMap Text Text)
customFields = filterWithKey (\k _ -> isPrefixOf "customfield_" k)
