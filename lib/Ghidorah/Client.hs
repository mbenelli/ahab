{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Ghidorah.Client where

import Data.Default (Default(def))
import Data.Proxy (Proxy(..))
import Data.Text as T
import Network.Connection (TLSSettings(TLSSettings))
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManager)
import Network.TLS (credentialLoadX509, onCertificateRequest,
                    onServerCertificate, clientHooks, clientSupported,
                    supportedCiphers, defaultParamsClient) 
import Network.TLS.Extra.Cipher (ciphersuite_strong)
import Servant.API (JSON, Header, QueryParam, Capture,
                    type (:>), type (:<|>)(..), Get)
import Servant.Client (client, mkClientEnv, runClientM, parseBaseUrl, ClientM)
import Text.Pretty.Simple

import Ghidorah.Config
import Ghidorah.Types


-- Certificate handling


mkMngr :: Text -> Text -> Text-> IO Manager
mkMngr hostName crtFile keyFile = do
  creds <- either error Just `fmap` credentialLoadX509
    (unpack crtFile) (unpack keyFile)
  let hooks = def { onCertificateRequest = \_ -> return creds
                  -- FIXME: it bypass server validation
                  , onServerCertificate = \_ _ _ _ -> return []
                  }
      clientParams = (defaultParamsClient (unpack hostName) "")
                     { clientHooks = hooks
                     , clientSupported = def
                       { supportedCiphers = ciphersuite_strong }
                     }
      tlsSettings = TLSSettings clientParams
  newManager $ mkManagerSettings tlsSettings Nothing


-- API

type API = "rest" :> "api" :> "2" :> "search"
  :> QueryParam "jql" Text
  :> QueryParam "maxResults" Int
  :> QueryParam "expand" Text
  :> QueryParam "fields" Text
  :> QueryParam "fieldsByKeys" Bool
  :> Header "X-AUSERNAME" Text 
  :> Header "Authorization" Text
  :> Get '[JSON] SearchResponse
  :<|>
  "rest" :> "api" :> "2" :> "field"
  :> Header "X-AUSERNAME" Text
  :> Header "Authorization" Text
  :> Get '[JSON] [FieldDetails]
  :<|>
  "rest" :> "api" :> "2" :> "issue" :> Capture "issueid" Text
  :> Header "X-AUSERNAME" Text
  :> Header "Authorization" Text
  :> Get '[JSON] IssueBean
  :<|>
  "rest" :> "api" :> "2" :> "issue" :> Capture "issueid" Text :> "changelog"
  :> Header "X-AUSERNAME" Text
  :> Header "Authorization" Text
  :> Get '[JSON] PageBeanChangelog

api :: Proxy API
api = Proxy

search :: Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Bool
       -> Maybe Text -> Maybe Text
       -> ClientM SearchResponse

getFields :: Maybe Text -> Maybe Text -> ClientM [FieldDetails]

issue :: Text -> Maybe Text -> Maybe Text -> ClientM IssueBean

changelogs :: Text -> Maybe Text -> Maybe Text -> ClientM PageBeanChangelog

search :<|> getFields :<|> issue :<|> changelogs = client api

auth :: Config -> Text
auth c = T.unwords [authorization c, token c]

query :: Text -> Text -> Config -> ClientM SearchResponse
query q f cfg = search (Just q) (Just 100) (Just "changelog")
                       (Just f) (Just True)
                       (Just $ user cfg) (Just $ auth cfg)

fieldsQuery :: Config -> ClientM [FieldDetails]
fieldsQuery cfg = getFields
  (Just $ user cfg) (Just $ auth cfg)

issueQuery :: Text -> Config -> ClientM IssueBean
issueQuery x cfg = issue x
  (Just $ user cfg) (Just $ auth cfg)

changelogQuery :: Text -> Config -> ClientM PageBeanChangelog
changelogQuery x cfg = changelogs x
  (Just $ user cfg) (Just $ auth cfg)



run :: (Config -> ClientM a)  -> IO (Either Text a)
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
      u <- parseBaseUrl  $ unpack (url cfg)
      let g = f cfg
      res <- runClientM g (mkClientEnv manager' u)
      case res of
        Left err -> return $ Left $ append "Error: " $ pack $ show err
        Right r -> return $ Right r


-- Printers

withContinuation :: Show b => (Config -> ClientM a) -> (a -> b) -> IO ()
withContinuation f k = do
  res <- run f
  case res of
    Left l -> putStrLn $ unpack l
    Right r -> pPrintNoColor $ k r

withSearchResult :: Show a => Text -> Text -> (SearchResponse -> a) -> IO ()
withSearchResult q f = withContinuation $ query q f

withFields :: Show a => ([FieldDetails] -> a) -> IO ()
withFields = withContinuation fieldsQuery

withIssue :: Show a => Text -> (IssueBean -> a) -> IO ()
withIssue x = withContinuation $ issueQuery x

withChangelog :: Show a => Text -> (PageBeanChangelog -> a) -> IO ()
withChangelog x = withContinuation $ changelogQuery x

