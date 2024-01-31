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



-- API call

data SearchResponse = SearchResponse
  { expand :: !String
  , issues :: !String -- JSON
  , maxResult :: !Int
  , names :: !String  -- JSON
  , schema :: !String -- JSON
  , startAt :: !Integer
  , total :: !Integer
  , warningMessage :: ![String]
  } deriving (Show, Generic)

instance FromJSON SearchResponse


type API = "rest" :> "api" :> "2" :> "search"
  :> QueryParam "jql" String :> Header "Authorization" String :> Get '[JSON] SearchResponse


api :: Proxy API
api = Proxy

search :: Maybe String -> Maybe String -> ClientM SearchResponse
search = client api

query :: String -> String -> ClientM SearchResponse
query q t = search (Just q) (Just ("Bearer " ++ t))

run' :: Config -> IO ()
run' cfg = do
  manager' <- mkMngr "issue.swf.daimler.com" (crtPath cfg) (keyPath cfg)
  u <- parseBaseUrl (url cfg) 
  res <- runClientM (query "type = Epic" (token cfg)) (mkClientEnv manager' u)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right s -> do
      print (total s)

run :: IO ()
run = do
  f <- defaultConfigFile
  cfg <- readConfig f -- readConfig "config.yaml"
  case cfg of
    Left e -> putStrLn $ "Error: " ++ show e
    Right c -> run' c

