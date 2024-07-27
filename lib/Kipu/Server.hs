{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Kipu.Server
-- Description: Web Server
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
--
-- Simple web server inteded to be used for handling webhooks.
-- Still a work in progress.
module Kipu.Server where

import Data.Aeson
import Data.Text
import GHC.Generics
import Kipu.Jira.CustomTypes (IssueBean)
import Kipu.Jira.Types (Changelog, UserDetails)
import Network.Wai.Handler.Warp
import Servant

data IssueRequest = IssueRequest
  { timestamp :: !(Maybe Text),
    webhookEvent :: !(Maybe Text),
    issue_event_type_name :: !(Maybe Text),
    user :: !(Maybe UserDetails),
    issue :: !(Maybe IssueBean),
    changelog :: !(Maybe Changelog)
  }
  deriving (Show, Generic)

instance FromJSON IssueRequest

type API = "issue" :> ReqBody '[JSON] IssueRequest :> Post '[PlainText] Text

processRequest :: IssueRequest -> Handler Text
processRequest x = return $ pack $ show x

api :: Proxy API
api = Proxy

server :: Server API
server = processRequest

app :: Application
app = serve api server

run :: IO ()
run = Network.Wai.Handler.Warp.run 8081 app
