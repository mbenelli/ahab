{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Client where

import Control.Monad.Reader (asks, runReader, Reader)
import Data.Default (Default(def))
import Data.Proxy (Proxy(..))
import Network.Connection (TLSSettings(TLSSettings))
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManager)
import Network.TLS (credentialLoadX509, onCertificateRequest, onServerCertificate,
                    clientHooks, clientSupported, supportedCiphers, defaultParamsClient) 
import Network.TLS.Extra.Cipher (ciphersuite_strong)
import Servant.API (JSON, Header, QueryParam, type (:>), type (:<|>)(..), Get)
import Servant.Client (client, mkClientEnv, runClientM, parseBaseUrl, ClientM)

import Config
import Search
import Field


-- Certificate handling


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

type API = "rest" :> "api" :> "2" :> "search"
  :> QueryParam "jql" String
  :> QueryParam "maxResults" Int
  :> QueryParam "fields" String
  :> QueryParam "fieldsByKeys" Bool
  :> Header "X-AUSERNAME" String
  :> Header "Authorization" String :> Get '[JSON] SearchResponse
  :<|>
  "rest" :> "api" :> "2" :> "field"
  :> Header "X-AUSERNAME" String
  :> Header "Authorization" String :> Get '[JSON] [FieldDetails]

api :: Proxy API
api = Proxy

search :: Maybe String -> Maybe Int -> Maybe String -> Maybe Bool -> Maybe String -> Maybe String -> ClientM SearchResponse

getFields :: Maybe String -> Maybe String -> ClientM [FieldDetails]

search :<|> getFields = client api

query :: String -> Config -> ClientM SearchResponse
query q cfg = search (Just q) Nothing (Just "*all") (Just True)
  (Just $ user cfg) (Just $ authorization cfg ++ " " ++ token cfg)


fieldsQuery :: Config -> ClientM [FieldDetails]
fieldsQuery cfg = getFields
  (Just $ user cfg) (Just $ authorization cfg ++ " " ++ token cfg)


run :: Reader Config (ClientM a)  -> IO (Either String a)
run f = do
  file <- defaultConfigFile
  c <- readConfig file
  case c of
    Left e -> return $ Left $ "Error: " ++ show e
    Right cfg -> do
      manager' <- case crtPath cfg of
        Just cert -> case keyPath cfg of
              Just ckey -> mkMngr (url cfg) cert ckey
              Nothing -> newTlsManager
        Nothing -> newTlsManager
      u <- parseBaseUrl (url cfg)
      let g = runReader f cfg
      res <- runClientM g (mkClientEnv manager' u)
      case res of
        Left err -> return $ Left $ "Error: " ++ show err
        Right r -> return $ Right r


-- Printers

withSearchResult :: Show a => String -> (SearchResponse -> a) -> IO ()
withSearchResult q f = do
  r <- run $ asks $ query q
  case r of
    Left l -> putStrLn l
    Right x -> print $ f x

withFields :: Show a => ([FieldDetails] -> a) -> IO ()
withFields f = do
  r <- run $ asks fieldsQuery
  case r of
    Left l -> putStrLn l
    Right x -> print $ f x

  

