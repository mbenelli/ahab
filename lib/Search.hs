{-# LANGUAGE DeriveGeneric #-}

module Search where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

import Field (Field)

data IncludedFields = IncludedFields
  { actuallyIncluded :: ![String]
  , excluded :: ![String]
  , includede :: ![String]
  } deriving (Show, Generic)

instance FromJSON IncludedFields


--data Field = Field
--  { self :: !String
--  , value :: !String
--  --, id :: !String
--  , disabled :: !Bool
--  } deriving (Show, Generic)



data IssueBean = IssueBean
  {
  -- changelog :: !String -- PageOfChangelogs
  --, editmeta :: !String -- IssueUpdateMetadata
  --, expand_ :: !String
  fields :: !(Maybe [Field])
  , fieldsToInclude :: !(Maybe IncludedFields)
  --,
  , id :: !String
  , key :: !String
  --, names_ :: !String -- JSON
  --, operation :: !String -- Operations
  --, properties :: !String -- JSON
  --, renderedFields :: !String -- JSON
  --, schema_ :: !String -- JSON
  --, self :: !String
  --, transitions :: !String -- [IssueTransition]
  --, versionedRepresentation :: !String -- JSON
  } deriving (Show, Generic)

instance FromJSON IssueBean

data SearchResponse = SearchResponse
  { expand :: !String
  , issues :: ![IssueBean]
  , maxResults :: !Int
--  , names :: !String  -- JSON
--  , schema :: !String -- JSON
  , startAt :: !Integer
  , total :: !Integer
  , warningMessage :: !(Maybe [String])
  } deriving (Show, Generic)

instance FromJSON SearchResponse



