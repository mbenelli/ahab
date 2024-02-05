{-# LANGUAGE DeriveGeneric #-}

module Ghidorah.Search where

import Data.Aeson (FromJSON)
import Data.Text
import GHC.Generics (Generic)

import Ghidorah.Field (Field)

data IncludedFields = IncludedFields
  { actuallyIncluded :: ![Text]
  , excluded :: ![Text]
  , includede :: ![Text]
  } deriving (Show, Generic)

instance FromJSON IncludedFields


--data Field = Field
--  { self :: !Text
--  , value :: !Text
--  --, id :: !Text
--  , disabled :: !Bool
--  } deriving (Show, Generic)



data IssueBean = IssueBean
  {
  -- changelog :: !Text -- PageOfChangelogs
  --, editmeta :: !Text -- IssueUpdateMetadata
  --, expand_ :: !Text
  fields :: !(Maybe [Field])
  , fieldsToInclude :: !(Maybe IncludedFields)
  --,
  , id :: !Text
  , key :: !Text
  --, names_ :: !Text -- JSON
  --, operation :: !Text -- Operations
  --, properties :: !Text -- JSON
  --, renderedFields :: !Text -- JSON
  --, schema_ :: !Text -- JSON
  --, self :: !Text
  --, transitions :: !Text -- [IssueTransition]
  --, versionedRepresentation :: !Text -- JSON
  } deriving (Show, Generic)

instance FromJSON IssueBean

data SearchResponse = SearchResponse
  { expand :: !Text
  , issues :: ![IssueBean]
  , maxResults :: !Int
--  , names :: !Text  -- JSON
--  , schema :: !Text -- JSON
  , startAt :: !Integer
  , total :: !Integer
  , warningMessage :: !(Maybe [Text])
  } deriving (Show, Generic)

instance FromJSON SearchResponse



