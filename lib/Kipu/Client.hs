{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module: Kipu.Client
-- Description: HTTP REST client
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
module Kipu.Client where

import BasicPrelude
import Data.Default (Default (def))
import Data.Text (append, pack, unpack)
import Kipu.Config
import Kipu.Jira.Api
import Kipu.Jira.CustomTypes
import qualified Kipu.Jira.InsightTypes as IT
import qualified Kipu.Jira.Types as JT
import Network.Connection (TLSSettings (TLSSettings))
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManager)
import Network.TLS
  ( clientHooks,
    clientSupported,
    credentialLoadX509,
    defaultParamsClient,
    onCertificateRequest,
    onServerCertificate,
    supportedCiphers,
  )
import Network.TLS.Extra.Cipher (ciphersuite_strong)
import Servant.Client (ClientM, mkClientEnv, parseBaseUrl, runClientM)
import Text.Pretty.Simple

-- Certificate handling

mkMngr :: Text -> Text -> Text -> IO Manager
mkMngr hostName crtFile keyFile = do
  creds <-
    either error Just
      `fmap` credentialLoadX509
        (unpack crtFile)
        (unpack keyFile)
  let hooks =
        def
          { onCertificateRequest = \_ -> return creds,
            -- FIXME: it bypass server validation
            onServerCertificate = \_ _ _ _ -> return []
          }
      clientParams =
        (defaultParamsClient (unpack hostName) "")
          { clientHooks = hooks,
            clientSupported =
              def
                { supportedCiphers = ciphersuite_strong
                }
          }
      tlsSettings = TLSSettings clientParams
  newManager $ mkManagerSettings tlsSettings Nothing

run :: (Config -> ClientM a) -> IO (Either Text a)
run f = do
  file <- defaultConfigFile
  c <- readConfig file
  case c of
    Left e -> return $ Left $ append "Error: " $ pack $ show e
    Right cfg -> do
      manager' <- case crtPath cfg of
        Just cert -> case keyPath cfg of
          Just ckey -> mkMngr (url cfg) cert ckey
          Nothing -> newTlsManager
        Nothing -> newTlsManager
      u <- parseBaseUrl $ unpack (url cfg)
      let g = f cfg
      res <- runClientM g (mkClientEnv manager' u)
      case res of
        Left err -> return $ Left $ append "Error: " $ pack $ show err
        Right r -> return $ Right r

-- Jira most useful fields
--

defaultFields :: [Text]
defaultFields =
  [ "id",
    "key",
    "project",
    "issuetype",
    "summary",
    "status",
    "created",
    "creator",
    "priority",
    "description",
    "assignee",
    "reporter",
    "fixVersions",
    "versions",
    "components",
    "issuelink",
    "resolution",
    "resolutiondate"
  ]

-- Printers

withContinuation :: (Show b) => (Config -> ClientM a) -> (a -> b) -> IO ()
withContinuation f k = do
  res <- run f
  case res of
    Left l -> putStrLn l
    Right r -> pPrintNoColor $ k r

withSearchResult ::
  (Show a) =>
  Text ->
  Int ->
  [Text] ->
  (SearchResponse -> a) ->
  IO ()
withSearchResult q i fs = withContinuation $ searchQuery q i fs

withFields :: (Show a) => ([JT.FieldDetails] -> a) -> IO ()
withFields = withContinuation fieldsQuery

withIssueTypes :: (Show a) => ([JT.IssueTypeDetails] -> a) -> IO ()
withIssueTypes = withContinuation issueTypeQuery

withIssue :: (Show a) => Text -> (IssueBean -> a) -> IO ()
withIssue x = withContinuation $ issueQuery x

withChangelog :: (Show a) => Text -> (JT.PageBeanChangelog -> a) -> IO ()
withChangelog x = withContinuation $ changelogQuery x

createIssue :: CreateIssueRequest -> IO ()
createIssue x = withContinuation (createIssue' x) id

withWorkspaceid :: (Show a) => (Text -> a) -> IO ()
withWorkspaceid = withContinuation workspaceidQuery

withAssetSearchResult :: (Show a) => Text -> Int -> (InsightSearchResponse -> a) -> IO ()
withAssetSearchResult q i = withContinuation $ assetQuery q i

collectSearchResult ::
  Text ->
  Int ->
  [IssueBean] ->
  IO (Either Text [IssueBean])
collectSearchResult q i xs = do
  res <- run $ searchQuery q i defaultFields
  case res of
    Left e -> return $ Left e
    Right r ->
      if null (issues r)
        then return $ Right xs
        else
          collectSearchResult
            q
            (length (xs ++ (issues r)))
            (xs ++ (issues r))

collectAssetResult :: Text -> Int -> [IT.ObjectEntry] -> IO (Either Text [IT.ObjectEntry])
collectAssetResult q i xs = do
  res <- run $ assetQuery q i
  case res of
    Left e -> return $ Left e
    Right r ->
      let entries = (insightSearchResponse_objectEntries r)
       in if null entries
            then return $ Right xs
            else
              collectAssetResult
                q
                ( i
                    + 1 -- (length (xs ++ entries))
                )
                (xs ++ entries)

-- | Collect search results with a maximum number of items to collect.
collectSearchResult' :: Text -> Int -> Int -> [IssueBean] -> IO (Either Text [IssueBean])
collectSearchResult' q i j xs = do
  res <- run $ searchQuery q i defaultFields
  case res of
    Left e -> return $ Left e
    Right r ->
      if length xs >= j
        then return $ Right xs
        else collectSearchResult' q (length (xs ++ (issues r))) j (xs ++ (issues r))
