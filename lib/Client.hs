{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Client where

import Data.Aeson (FromJSON)
import Data.Default (Default(def))
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import Network.Connection (TLSSettings(TLSSettings))
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.TLS (credentialLoadX509, onCertificateRequest, onServerCertificate,
                    clientHooks, clientSupported, supportedCiphers, defaultParamsClient) 
import Network.TLS.Extra.Cipher (ciphersuite_strong)
import Servant.API (JSON, Header, QueryParam, type (:>), Get)
import Servant.Client (client, mkClientEnv, runClientM, parseBaseUrl, ClientM)

import Config
import Field

-- Certificate handling

--crtPath :: FilePath
--crtPath = "tls.crt"
--
--keyPath :: FilePath
--keyPath = "tls.key"

mkMngr :: String ->  FilePath -> FilePath -> IO Manager
mkMngr hostName crtFile keyFile = do
  creds <- either error Just `fmap` credentialLoadX509 crtFile keyFile
  let hooks = def { onCertificateRequest = \_ -> return creds
                  , onServerCertificate = \_ _ _ _ -> return []  -- FIXME: it bypass server validation
                  }
      clientParams = (defaultParamsClient hostName "")
                     { clientHooks = hooks
                     , clientSupported = def { supportedCiphers = ciphersuite_strong }
                     }
      tlsSettings = TLSSettings clientParams
  newManager $ mkManagerSettings tlsSettings Nothing



-- API

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


type API = "rest" :> "api" :> "2" :> "search"
  :> QueryParam "jql" String
  :> QueryParam "maxResults" Int
  :> QueryParam "fields" String
  :> QueryParam "fieldsByKeys" Bool
  :> Header "Authorization" String :> Get '[JSON] SearchResponse


api :: Proxy API
api = Proxy

search :: Maybe String -> Maybe Int -> Maybe String -> Maybe Bool -> Maybe String -> ClientM SearchResponse
search = client api

query :: String -> String -> ClientM SearchResponse
query q t = search (Just q) (Just 1) (Just "*all") (Just True) (Just ("Bearer " ++ t))

run' :: Config -> IO ()
run' cfg = do
  manager' <- mkMngr (url cfg) (crtPath cfg) (keyPath cfg)
  u <- parseBaseUrl (url cfg) 
  res <- runClientM (query "type = Story" (token cfg)) (mkClientEnv manager' u)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right s -> print s

run :: IO ()
run = do
  f <- defaultConfigFile
  cfg <- readConfig f -- readConfig "config.yaml"
  case cfg of
    Left e -> putStrLn $ "Error: " ++ show e
    Right c -> run' c

