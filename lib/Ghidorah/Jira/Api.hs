{-# LANGUAGE DeriveGeneric #-}

module Ghidorah.Jira.Api where

import BasicPrelude
import Data.Aeson
import GHC.Generics

import Ghidorah.Jira.CustomTypes (IssueBean, IssueCoreObject)

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

data CreateIssueRequest = CreateIssueRequest
   { fields :: IssueCoreObject
   } deriving (Show, Generic)
 
instance ToJSON CreateIssueRequest

