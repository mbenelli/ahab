{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module: Ahab.Jira.Types
-- Description: Jira types
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
module Ahab.Jira.Types where

import BasicPrelude
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict as M
import Data.Text (pack, splitOn, unpack)
import GHC.Generics (Generic)

fieldModifier :: String -> String
fieldModifier = unpack . last . splitOn "_" . pack

options :: Options
options = defaultOptions {fieldLabelModifier = fieldModifier}

data JsonTypeBean = JsonTypeBean
  { jsonTypeBean_type :: !Text,
    jsonTypeBean_configuration :: !(Maybe Object),
    jsonTypeBean_custom :: !(Maybe Text),
    jsonTypeBean_customId :: !(Maybe Int),
    jsonTypeBean_items :: !(Maybe Text),
    jsonTypeBean_system :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON JsonTypeBean where
  parseJSON = genericParseJSON options

instance ToJSON JsonTypeBean where
  toJSON = genericToJSON options

data UpdateProjectCategory = UpdateProjectCategory
  { updateProjectCategory_id :: !Text,
    updateProjectCategory_name :: !Text,
    updateProjectCategory_self :: !Text,
    updateProjectCategory_description :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON UpdateProjectCategory where
  parseJSON = genericParseJSON options

instance ToJSON UpdateProjectCategory where
  toJSON = genericToJSON options

data ProjectDetails = ProjectDetails
  { projectDetails_avatarUrls :: !(Maybe AvatarUrlsBean),
    projectDetails_id :: !Text,
    projectDetails_key :: !Text,
    projectDetails_name :: !Text,
    projectDetails_projectCategory :: !(Maybe UpdateProjectCategory),
    projectDetails_projectTypeKey :: !(Maybe Text),
    projectDetails_self :: !(Maybe Text),
    projectDetails_simplified :: !(Maybe Bool)
  }
  deriving (Show, Generic)

instance FromJSON ProjectDetails where
  parseJSON = genericParseJSON options

instance ToJSON ProjectDetails where
  toJSON = genericToJSON options

data Scope = Scope
  { scope_project :: !ProjectDetails,
    scope_type :: !Text
  }
  deriving (Show, Generic)

instance FromJSON Scope where
  parseJSON = genericParseJSON options

instance ToJSON Scope where
  toJSON = genericToJSON options

data Field = Field
  { fieldId' :: !Text,
    fieldName :: !(Maybe Text),
    fieldValue :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance {-# OVERLAPPING #-} FromJSON [Field] where
  parseJSON x =
    parseJSON x
      >>= mapM parseField
      . toList
      . M.filter
        ( \case
            Object _ -> True
            _ -> False
        )

parseField :: (Text, Value) -> Parser Field
parseField (i, v) =
  withObject
    "field body"
    ( \o ->
        Field i <$> o .:? "name" <*> o .:? "value"
    )
    v

instance ToJSON Field where
  toEncoding =
    genericToEncoding
      defaultOptions
        { fieldLabelModifier = dropWhileEnd $ (==) '\''
        }

data FieldDetails = FieldDetails
  { fieldDetails_clausesNames :: !(Maybe [Text]),
    fieldDetails_custom :: !(Maybe Bool),
    fieldDetails_id :: !Text,
    fieldDetails_key :: !Text,
    fieldDetails_name :: !Text,
    fieldDetails_navigable :: !Bool,
    fieldDetails_orderable :: !Bool,
    fieldDetails_searchable :: !Bool,
    fieldDetails_schema :: !(Maybe JsonTypeBean),
    fieldDetails_scope :: !(Maybe Scope)
  }
  deriving (Show, Generic)

instance FromJSON FieldDetails where
  parseJSON = genericParseJSON options

instance ToJSON FieldDetails where
  toJSON = genericToJSON options

data IncludedFields = IncludedFields
  { includedFields_actuallyIncluded :: ![Text],
    includedFields_excluded :: ![Text],
    includedFields_includede :: ![Text]
  }
  deriving (Show, Generic)

instance FromJSON IncludedFields where
  parseJSON = genericParseJSON options

instance ToJSON IncludedFields where
  toJSON = genericToJSON options

data ChangeDetails = ChangeDetails
  { changeDetails_field :: !(Maybe Text),
    changeDetails_fieldId :: !(Maybe Text),
    changeDetails_fieldtype :: !(Maybe Text),
    changeDetails_from :: !(Maybe Text),
    changeDetails_fromString :: !(Maybe Text),
    changeDetails_to :: !(Maybe Text),
    changeDetails_toString :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON ChangeDetails where
  parseJSON = genericParseJSON options

instance ToJSON ChangeDetails where
  toJSON = genericToJSON options

data HistoryMetadataPartecipant = HistoryMetadataPartecipant
  { historyMetadataPartecipant_avatarUrl :: !(Maybe Text),
    historyMetadataPartecipant_displayName :: !(Maybe Text),
    historyMetadataPartecipant_displayNameKey :: !(Maybe Text),
    historyMetadataPartecipant_id :: !(Maybe Text),
    historyMetadataPartecipant_type :: !(Maybe Text),
    historyMetadataPartecipant_url :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON HistoryMetadataPartecipant where
  parseJSON = genericParseJSON options

instance ToJSON HistoryMetadataPartecipant where
  toJSON = genericToJSON options

data HistoryMetadata = HistoryMetadata
  { historyMetada_activityDescription :: !(Maybe Text),
    historyMetada_activityDescriptionKey :: !(Maybe Text),
    historyMetada_actor :: !(Maybe HistoryMetadataPartecipant),
    historyMetada_cause :: !(Maybe HistoryMetadataPartecipant),
    historyMetada_description :: !(Maybe Text),
    historyMetada_descriptionKey :: !(Maybe Text),
    historyMetada_emailDescription :: !(Maybe Text),
    historyMetada_emailDescriptionKey :: !(Maybe Text),
    historyMetada_generator :: !(Maybe HistoryMetadataPartecipant)
  }
  deriving (Show, Generic)

instance FromJSON HistoryMetadata where
  parseJSON = genericParseJSON options

instance ToJSON HistoryMetadata where
  toJSON = genericToJSON options

data AvatarUrlsBean = AvatarUrlsBean
  { avatarUrlsBean_avatar16x16 :: !(Maybe Text),
    avatarUrlsBean_avatar24x24 :: !(Maybe Text),
    avatarUrlsBean_avatar32x32 :: !(Maybe Text),
    avatarUrlsBean_avatar48x48 :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON AvatarUrlsBean where
  parseJSON = genericParseJSON options

instance ToJSON AvatarUrlsBean where
  toJSON = genericToJSON options

data UserDetails = UserDetail
  { userDetails_accountId :: !(Maybe Text),
    userDetails_accountType :: !(Maybe Text),
    userDetails_active :: !(Maybe Bool),
    userDetails_avatarUrls :: !(Maybe AvatarUrlsBean),
    userDetails_displayName :: !(Maybe Text),
    userDetails_emailAddress :: !(Maybe Text),
    userDetails_key :: !(Maybe Text),
    userDetails_name :: !(Maybe Text),
    userDetails_self :: !(Maybe Text),
    userDetails_timeZone :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON UserDetails where
  parseJSON = genericParseJSON options

instance ToJSON UserDetails where
  toJSON = genericToJSON options

data Changelog = Changelog
  { changelog_author :: !(Maybe UserDetails),
    changelog_created :: !Text,
    changelog_historyMetadata :: !(Maybe HistoryMetadata),
    changelog_id :: !(Maybe Text),
    changelog_items :: !(Maybe [ChangeDetails])
  }
  deriving (Show, Generic)

instance FromJSON Changelog where
  parseJSON = genericParseJSON options

instance ToJSON Changelog where
  toJSON = genericToJSON options

data PageBeanChangelog = PageBeanChangelog
  { pageBeanChangelog_isLast :: !(Maybe Bool), -- wether is last page
    pageBeanChangelog_maxResults :: !(Maybe Int),
    pageBeanChangelog_nextPage :: !(Maybe Text), -- uri of next page
    pageBeanChangelog_self :: !(Maybe Text),
    pageBeanChangelog_startAt :: !(Maybe Int),
    pageBeanChangelog_total :: !(Maybe Int),
    pageBeanChangelog_values :: !(Maybe [Changelog])
  }
  deriving (Show, Generic)

instance FromJSON PageBeanChangelog where
  parseJSON = genericParseJSON options

instance ToJSON PageBeanChangelog where
  toJSON = genericToJSON options

data PageOfChangelogs = PageOfChangelogs
  { pageOfChangelogs_histories :: !(Maybe [Changelog]),
    pageOfChangelogs_maxResults :: !(Maybe Int),
    pageOfChangelogs_startAt :: !(Maybe Int),
    pageOfChangelogs_total :: !(Maybe Int)
  }
  deriving (Show, Generic)

instance FromJSON PageOfChangelogs where
  parseJSON = genericParseJSON options

instance ToJSON PageOfChangelogs where
  toJSON = genericToJSON options

data IssueEvent = IssueEvent
  { issueEvent_id :: !Int,
    issueName_name :: !Text
  }

data Priority = Priority
  { priority_id :: !Text,
    priority_name :: !Text,
    priority_self :: !Text,
    priority_iconUrl :: !Text
  }
  deriving (Show, Generic)

instance FromJSON Priority where
  parseJSON = genericParseJSON options

instance ToJSON Priority where
  toJSON = genericToJSON options

data Component = Component
  { component_id :: !Text,
    component_name :: !Text,
    component_self :: !Text
  }
  deriving (Show, Generic)

instance FromJSON Component where
  parseJSON = genericParseJSON options

instance ToJSON Component where
  toJSON = genericToJSON options

data Resolution = Resolution
  { resolution_id :: !Text,
    resolution_name :: !Text,
    resolution_description :: !Text,
    resolution_self :: !Text
  }
  deriving (Show, Generic)

instance FromJSON Resolution where
  parseJSON = genericParseJSON options

instance ToJSON Resolution where
  toJSON = genericToJSON options

data StatusCategory = StatusCategory
  { statusCategory_id :: !Int,
    statusCategory_key :: !Text,
    statusCategory_name :: !Text,
    statusCategory_self :: !Text
  }
  deriving (Show, Generic)

instance FromJSON StatusCategory where
  parseJSON = genericParseJSON options

instance ToJSON StatusCategory where
  toJSON = genericToJSON options

data Status = Status
  { status_id :: !Text,
    status_name :: !Text,
    status_description :: !Text,
    status_self :: !Text,
    status_iconUrl :: !Text,
    status_statusCategory :: !StatusCategory
  }
  deriving (Show, Generic)

instance FromJSON Status where
  parseJSON = genericParseJSON options

instance ToJSON Status where
  toJSON = genericToJSON options

data Version = Version
  { version_id :: !Text,
    version_name :: !Text,
    version_description :: !(Maybe Text),
    version_releaseDate :: !(Maybe Text),
    version_released :: !(Maybe Bool),
    version_archived :: !(Maybe Bool),
    version_self :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON Version where
  parseJSON = genericParseJSON options

instance ToJSON Version where
  toJSON = genericToJSON options

data IssueTypeDetails = IssueTypeDetails
  { issueTypeDetails_avatarId :: !(Maybe Int),
    issueTypeDetails_description :: !(Maybe Text),
    issueTypeDetails_entityId :: !(Maybe Text),
    issueTypeDetails_hierarchyLevel :: !(Maybe Int),
    issueTypeDetails_iconUrl :: !(Maybe Text),
    issueTypeDetails_id :: !(Maybe Text),
    issueTypeDetails_name :: !(Maybe Text),
    issueTypeDetails_scope :: !(Maybe Scope),
    issueTypeDetails_self :: !(Maybe Text),
    issueTypeDetails_subtask :: !(Maybe Bool)
  }
  deriving (Show, Generic)

instance FromJSON IssueTypeDetails where
  parseJSON = genericParseJSON options

instance ToJSON IssueTypeDetails where
  toJSON = genericToJSON options

-- For creating a Issue

data IssueType = IssueType
  { issueType_id :: !Text
  }
  deriving (Show, Generic)

instance ToJSON IssueType where
  toEncoding = genericToEncoding options

data Project = Project
  { project_id :: !Text,
    project_key :: !Text,
    project_name :: !Text
  }
  deriving (Show, Generic)

instance ToJSON Project where
  toEncoding = genericToEncoding options

instance FromJSON Project where
  parseJSON = genericParseJSON options

-- Issue Description and Comments

data Content = Content
  { condent_type :: !Text,
    content_text :: !Text
  }

data Body = Body
  { body_type :: !Text,
    body_content :: ![Content]
  }

data Description = Description
  { description_type :: !String,
    description_version :: !Int,
    description_content :: ![Body]
  }

data Comment = Comment
  { comment_author :: !UserDetails,
    comment_body :: !Description,
    comment_created :: !Text,
    comment_id :: !Text,
    comment_self :: !Text,
    comment_updated :: !(Maybe Text),
    comment_updateAuthor :: !(Maybe UserDetails)
  }

-- Issue Links

data LinkType = LinkType
  { linkType_id :: !Text,
    linkType_name :: !Text,
    linkType_inward :: !Text,
    linkType_outward :: !Text
  }
  deriving (Show, Generic)

instance FromJSON LinkType where
  parseJSON = genericParseJSON options

instance ToJSON LinkType where
  toJSON = genericToJSON options

data LinkedIssue = LinkedIssue
  { linkedIssue_id :: !Text,
    linkedIssue_key :: !Text,
    linkedIssue_Status :: !Text
  }
  deriving (Show, Generic)

instance FromJSON LinkedIssue where
  parseJSON = genericParseJSON options

instance ToJSON LinkedIssue where
  toJSON = genericToJSON options

data IssueLink = IssueLink
  { issueLink_id :: !Text,
    issueLink_outwardIssue :: !(Maybe LinkedIssue),
    issueLink_inwardIssue :: !(Maybe LinkedIssue),
    issueLink_type :: !LinkType
  }
  deriving (Show, Generic)

instance FromJSON IssueLink where
  parseJSON = genericParseJSON options

instance ToJSON IssueLink where
  toJSON = genericToJSON options

data SubTask = SubTask
  { subTask_id :: !Text,
    subTask_outwardIssue :: !LinkedIssue,
    subTask_type :: !LinkType
  }
  deriving (Show, Generic)

instance FromJSON SubTask where
  parseJSON = genericParseJSON options

instance ToJSON SubTask where
  toJSON = genericToJSON options
