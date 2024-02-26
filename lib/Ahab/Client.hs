{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ahab.Client where

import Ahab.Config
import Ahab.Jira.Api
import Ahab.Jira.CustomTypes
import qualified Ahab.Jira.Types as JT
import BasicPrelude
import Data.Default (Default (def))
import Data.Text (append, pack, unpack)
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

collectSearchResult' :: Text -> Int -> Int -> [IssueBean] -> IO (Either Text [IssueBean])
collectSearchResult' q i j xs = do
  res <- run $ searchQuery q i defaultFields
  case res of
    Left e -> return $ Left e
    Right r ->
      if length xs >= j
        then return $ Right xs
        else collectSearchResult' q (length (xs ++ (issues r))) j (xs ++ (issues r))
