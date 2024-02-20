{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Ghidorah.Server where

import Data.Aeson
import Data.Text 
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant

import Ghidorah.Jira.Types (Changelog, IssueBean, UserDetail)


data IssueRequest = IssueRequest
  { timestamp :: !(Maybe Text)
  , webhookEvent :: !(Maybe Text)
  , issue_event_type_name :: !(Maybe Text)
  , user :: !(Maybe UserDetail)
  , issue :: !(Maybe IssueBean)
  , changelog :: !(Maybe Changelog)
} deriving (Show, Generic)

instance FromJSON IssueRequest

type API = "issue" :> ReqBody '[JSON] IssueRequest :> Post '[PlainText] Text

processRequest :: IssueRequest -> Text
processRequest x = pack $ show x

api :: Proxy API
api = Proxy

server :: Server API
server = return . processRequest

app :: Application
app = serve api server

run :: IO ()
run = Network.Wai.Handler.Warp.run 8081 app



