{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ghidorah.Types where

import BasicPrelude

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict as M
import GHC.Generics (Generic)

data Field = Field
  { fieldId' :: !Text
  , fieldName :: !(Maybe Text)
  , fieldValue :: !(Maybe Text)
  }
  deriving Show

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

data FieldDetails = FieldDetails
  { field_clausesNames :: !(Maybe [Text])
  , field_custom :: !(Maybe Bool)
  , field_id :: !(Maybe Text)
  , field_key :: !(Maybe Text)
  , field_name :: !(Maybe Text)
  , field_navigable :: !(Maybe Bool)
  , field_orderable :: !(Maybe Bool)
  , field_searchable :: !(Maybe Bool)
  }
  deriving (Show, Generic)

instance FromJSON FieldDetails where
  parseJSON = genericParseJSON defaultOptions{ fieldLabelModifier = drop 6 }


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
  parseJSON = genericParseJSON defaultOptions{ fieldLabelModifier = drop 4 }

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
  parseJSON = genericParseJSON defaultOptions{ fieldLabelModifier = drop 5 }

data Changelog = Changelog
  { changelog_author :: !(Maybe UserDetail)
  , changelog_created :: !Text
  , changelog_historyMetadata :: !(Maybe HistoryMetadata)
  , changelog_id :: !(Maybe Text)
  , changelog_items :: !(Maybe [ChangeDetails])
  } deriving (Show, Generic)

instance FromJSON Changelog where
  parseJSON = genericParseJSON defaultOptions{ fieldLabelModifier = drop 10 }

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
  parseJSON = genericParseJSON defaultOptions{ fieldLabelModifier = drop 4 }

data PageOfChangelogs = PageOfChangelogs
  { pog_histories :: !(Maybe [Changelog])
  , pog_maxResults :: !(Maybe Int)
  , pog_startAt :: !(Maybe Int)
  , pog_total :: !(Maybe Int)
  } deriving (Show, Generic)

instance FromJSON PageOfChangelogs where
  parseJSON = genericParseJSON defaultOptions{ fieldLabelModifier = drop 4 }


data IssueBean = IssueBean
  { issue_changelog :: !(Maybe PageOfChangelogs)
  , issue_editmeta :: !(Maybe Text) -- IssueUpdateMetadata
  , issue_expand :: !(Maybe Text)
  , issue_fields :: !(Maybe [Field])
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
  parseJSON = genericParseJSON defaultOptions{ fieldLabelModifier = drop 6 }

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

