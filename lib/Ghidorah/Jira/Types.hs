{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ghidorah.Jira.Types where

import BasicPrelude

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict as M
import Data.Text (splitOn, pack, unpack)
import GHC.Generics (Generic)

fieldModifier :: String -> String
fieldModifier  = unpack . last . splitOn "_" . pack

options :: Options
options = defaultOptions { fieldLabelModifier = fieldModifier }


data JsonTypeBean = JsonTypeBean
  { jsonTypeBean_type :: !Text
  , jsonTypeBean_configuration :: !(Maybe Object)
  , jsonTypeBean_custom :: !(Maybe Text)
  , jsonTypeBean_customId :: !(Maybe Int)
  , jsonTypeBean_items :: !(Maybe Text)
  , jsonTypeBean_system :: !(Maybe Text)
  } deriving (Show, Generic)

instance FromJSON JsonTypeBean where
  parseJSON = genericParseJSON options

data UpdateProjectCategory = UpdateProjectCategory
  { updateProjectCategory_id :: !Text
  , updateProjectCategory_name :: !Text
  , updateProjectCategory_self :: !Text
  , updateProjectCategory_description :: !(Maybe Text)
  } deriving (Show, Generic)

instance FromJSON UpdateProjectCategory where
  parseJSON = genericParseJSON options

data ProjectDetails = ProjectDetails
  { projectDetails_avatarUrls :: !(Maybe AvatarUrlsBean)
  , projectDetails_id :: !Text
  , projectDetails_key :: !Text
  , projectDetails_name :: !Text
  , projectDetails_projectCategory :: !(Maybe UpdateProjectCategory)
  , projectDetails_projectTypeKey :: !(Maybe Text)
  , projectDetails_self :: !(Maybe Text)
  , projectDetails_simplified :: !(Maybe Bool)
  } deriving (Show, Generic)

instance FromJSON ProjectDetails where
  parseJSON = genericParseJSON options

data Scope = Scope
  { scope_project :: !ProjectDetails
  , scope_type :: !Text
  } deriving (Show, Generic)

instance FromJSON Scope where
  parseJSON = genericParseJSON options

data Field = Field
  { fieldId' :: !Text
  , fieldName :: !(Maybe Text)
  , fieldValue :: !(Maybe Text)
  }  deriving (Show, Generic)

instance {-# OVERLAPPING #-} FromJSON [Field] where
  parseJSON x =
    parseJSON x >>= mapM parseField . toList . M.filter (\case
      Object _ -> True
      _ -> False)

parseField :: (Text, Value) -> Parser Field
parseField (i, v) =
  withObject "field body" (\ o ->
    Field i <$> o .:? "name" <*> o .:? "value")
    v

instance ToJSON Field where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = dropWhileEnd $ (==) '\''
  }

data FieldDetails = FieldDetails
  { fieldDetails_clausesNames :: !(Maybe [Text])
  , fieldDetails_custom :: !(Maybe Bool)
  , fieldDetails_id :: !Text
  , fieldDetails_key :: !Text
  , fieldDetails_name :: !Text
  , fieldDetails_navigable :: !Bool
  , fieldDetails_orderable :: !Bool
  , fieldDetails_searchable :: !Bool
  , fieldDetails_schema :: !(Maybe JsonTypeBean)
  , fieldDetails_scope :: !(Maybe Scope)
  }
  deriving (Show, Generic)

instance FromJSON FieldDetails where
  parseJSON = genericParseJSON options


data IncludedFields = IncludedFields
  { actuallyIncluded :: ![Text]
  , excluded :: ![Text]
  , includede :: ![Text]
  } deriving (Show, Generic)

instance FromJSON IncludedFields


data ChangeDetails = ChangeDetails
  { field :: !(Maybe Text)
  , fieldId :: !(Maybe Text)
  , fieldtype :: !(Maybe Text)
  , from :: !(Maybe Text)
  , fromString :: !(Maybe Text)
  , to :: !(Maybe Text)
  , toString :: !(Maybe Text)
  } deriving (Show, Generic)

instance FromJSON ChangeDetails

data HistoryMetadataPartecipant = HistoryMetadataPartecipant
  { hmp_avatarUrl :: !(Maybe Text)
  , hmp_displayName :: !(Maybe Text)
  , hmp_displayNameKey :: !(Maybe Text)
  , hmp_id :: !(Maybe Text)
  , hmp_type :: !(Maybe Text)
  , hmp_url :: !(Maybe Text)
  } deriving (Show, Generic)

instance FromJSON HistoryMetadataPartecipant where
  parseJSON = genericParseJSON options

data HistoryMetadata = HistoryMetadata
  { activityDescription :: !(Maybe Text)
  , activityDescriptionKey :: !(Maybe Text)
  , actor :: !(Maybe HistoryMetadataPartecipant)
  , cause :: !(Maybe HistoryMetadataPartecipant)
  , description :: !(Maybe Text)
  , descriptionKey :: !(Maybe Text)
  , emailDescription :: !(Maybe Text)
  , emailDescriptionKey :: !(Maybe Text)
  , generator :: !(Maybe HistoryMetadataPartecipant)
  } deriving (Show, Generic)

instance FromJSON HistoryMetadata

data AvatarUrlsBean = AvatarUrlsBean
  { avatar16x16 :: !(Maybe Text)
  , avatar24x24 :: !(Maybe Text)
  , avatar32x32 :: !(Maybe Text)
  , avatar48x48 :: !(Maybe Text)
  } deriving (Show, Generic)

instance FromJSON AvatarUrlsBean

data UserDetail = UserDetail
  { user_accountId :: !(Maybe Text)
  , user_accountType :: !(Maybe Text)
  , user_active :: !(Maybe Bool)
  , user_avatarUrls :: !(Maybe AvatarUrlsBean)
  , user_displayName :: !(Maybe Text)
  , user_emailAddress :: !(Maybe Text)
  , user_key :: !(Maybe Text)
  , user_name :: !(Maybe Text)
  , user_self :: !(Maybe Text)
  , user_timeZone :: !(Maybe Text)
  } deriving (Show, Generic)

instance FromJSON UserDetail where
  parseJSON = genericParseJSON options

data Changelog = Changelog
  { changelog_author :: !(Maybe UserDetail)
  , changelog_created :: !Text
  , changelog_historyMetadata :: !(Maybe HistoryMetadata)
  , changelog_id :: !(Maybe Text)
  , changelog_items :: !(Maybe [ChangeDetails])
  } deriving (Show, Generic)

instance FromJSON Changelog where
  parseJSON = genericParseJSON options

data PageBeanChangelog = PageBeanChangelog
  { pbc_isLast :: !(Maybe Bool)   -- wether is last page
  , pbc_maxResults :: !(Maybe Int)
  , pbc_nextPage :: !(Maybe Text) -- uri of next page
  , pbc_self :: !(Maybe Text)
  , pbc_startAt :: !(Maybe Int)
  , pbc_total :: !(Maybe Int)
  , pbc_values :: !(Maybe [Changelog])
  } deriving (Show, Generic)

instance FromJSON PageBeanChangelog where
  parseJSON = genericParseJSON options

data PageOfChangelogs = PageOfChangelogs
  { pog_histories :: !(Maybe [Changelog])
  , pog_maxResults :: !(Maybe Int)
  , pog_startAt :: !(Maybe Int)
  , pog_total :: !(Maybe Int)
  } deriving (Show, Generic)

instance FromJSON PageOfChangelogs where
  parseJSON = genericParseJSON options

data IssueEvent = IssueEvent
  { issueEvent_id :: !Int
  , issueName_name :: !Text
  }
data IssueBean = IssueBean
  { issue_changelog :: !(Maybe PageOfChangelogs)
  , issue_editmeta :: !(Maybe Text) -- IssueUpdateMetadata
  , issue_expand :: !(Maybe Text)
  , issue_fields :: !(Maybe Object)
  , issue_fieldsToInclude :: !(Maybe IncludedFields)
  --
  , issue_id :: !Text
  , issue_key :: !Text
  --, issue_names :: !(Maybe Object)
  --, issue_operation :: !Text -- Operations
  --, issue_properties :: !(Maybe Object)
  --, issue_renderedFields :: !(Maybe Object)
  --, issue_schema :: !(Maybe Object)
  --, issue_self :: !Text
  --, issue_transitions :: !Text -- [IssueTransition]
  --, issue_versionedRepresentation :: !(Maybe Object)
  } deriving (Show, Generic)

instance FromJSON IssueBean where
  parseJSON = genericParseJSON options

data SearchResponse = SearchResponse
  { expand :: !(Maybe Text)
  , issues :: ![IssueBean]
  , maxResults :: !Int
  , names :: !(Maybe Object)
  , schema :: !(Maybe Object)
  , startAt :: !Int
  , total :: !Int
  , warningMessage :: !(Maybe [Text])
  } deriving (Show, Generic)

instance FromJSON SearchResponse

data IssueTypeDetails = IssueTypeDetails
  { itd_avatarId :: !(Maybe Int)
  , itd_description :: !(Maybe Text)
  , itd_entityId :: !(Maybe Text)
  , itd_hierarchyLevel :: !(Maybe Int)
  , itd_iconUrl :: !(Maybe Text)
  , itd_id :: !(Maybe Text)
  , itd_name :: !(Maybe Text)
--  , itd_scope :: !(Maybe Scope)
  , itd_self :: !(Maybe Text)
  , itd_subtask :: !(Maybe Bool)
  } deriving (Show, Generic)

instance FromJSON IssueTypeDetails where
  parseJSON = genericParseJSON options

data IssueType = IssueType
  { issueType_id :: !Text
  } deriving (Show, Generic)

instance ToJSON IssueType where
  toEncoding = genericToEncoding options

data Project = Project
  { projcet_id :: !Text
  , project_key :: !Text
  , project_name :: !Text
  } deriving (Show, Generic)

instance ToJSON Project where
  toEncoding = genericToEncoding options

data IssueObject = IssueObject
  { i_summary :: !Text
  , i_description :: !(Maybe Text)
  , i_issuetype :: !IssueType
  , i_project :: !Project
  } deriving (Show, Generic) 

instance ToJSON IssueObject where
 toEncoding = genericToEncoding options

data CreateIssueRequest = CreateIssueRequest
   { createIssue_fields :: IssueObject
   } deriving (Show, Generic)
 
instance ToJSON CreateIssueRequest where
 toEncoding = genericToEncoding options

-- Issue Description and Comments

data Author = Author
  { accountId :: !Text
  , active :: !Bool
  , displayName :: !Text
  , self :: !Text
  }

data Content = Content
  { condent_type :: !Text
  , content_text :: !Text
  }

data Body = Body
  { body_type :: !Text
  , body_content :: ![Content]
  }

data Description = Description
  { description_type :: !String
  , description_version :: !Int
  , description_content :: ![Body]
  }

data Comment = Comment
  { comment_author :: !Author
  , comment_body :: !Description
  , comment_created :: !Text
  , comment_id :: !Text
  , comment_self :: !Text
  , comment_updated :: !(Maybe Text)
  , comment_updateAuthor :: !(Maybe Author)
  }

-- Issue Links

data LinkType = LinkType
  { linkType_id :: !Text
  , linkType_name :: !Text
  , linkType_inward :: !Text
  , linkType_outward :: !Text
  }

data LinkedIssue = LinkedIssue
  { linkedIssue_id :: !Text
  , linkedIssue_key :: !Text
  , linkedIssue_Status :: !Text
  }

data IssueLink = IssueLink
  { issueLink_id :: !Text
  , issueLink_outwardIssue :: !(Maybe LinkedIssue)
  , issueLink_inwardIssue :: !(Maybe LinkedIssue)
  , issueLink_type :: !LinkType
  }

data SubTask = SubTask
  { subTask_id :: !Text
  , subTask_outwardIssue :: !LinkedIssue
  , subTask_type :: !LinkType
  }


