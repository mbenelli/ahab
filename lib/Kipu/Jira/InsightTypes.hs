{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Additional types for Jira's Insight plugin
module Kipu.Jira.InsightTypes where

import BasicPrelude
import Data.Aeson
import GHC.Generics
import qualified Kipu.Jira.Types as JT

data ObjectEntry = ObjectEntry
  { objectEntry_id :: !Int,
    objectEntry_label :: !Text,
    objectEntry_name :: !Text,
    objectEntry_attributes :: ![Attribute],
    objectEntry_objectKey :: !Text,
    objectEntry_objectType :: !ObjectType
  }
  deriving (Show, Generic)

instance FromJSON ObjectEntry where
  parseJSON = genericParseJSON JT.options

data ObjectType = ObjectType
  { objectType_id :: !Int,
    objectType_name :: !Text
  }
  deriving (Show, Generic)

instance FromJSON ObjectType where
  parseJSON = genericParseJSON JT.options

data Attribute = Attribute
  { attribute_id :: !Int,
    attribute_objectAttributeValues :: ![AttributeValue],
    attribute_objectId :: !Int,
    attribute_objectTypeAttributeId :: !Int
  }
  deriving (Show, Generic)

instance FromJSON Attribute where
  parseJSON = genericParseJSON JT.options

data AttributeValue = AttributeValue
  { attributeValue_displayValue :: !Text,
    attributeValue_searchValue :: !Text
  }
  deriving (Show, Generic)

instance FromJSON AttributeValue where
  parseJSON = genericParseJSON JT.options
