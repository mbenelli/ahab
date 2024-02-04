{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Client where

import Control.Monad.Reader
import Data.Aeson (FromJSON)
import Data.Default (Default(def))
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import Network.Connection (TLSSettings(TLSSettings))
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManager)
import Network.TLS (credentialLoadX509, onCertificateRequest, onServerCertificate,
                    clientHooks, clientSupported, supportedCiphers, defaultParamsClient) 
import Network.TLS.Extra.Cipher (ciphersuite_strong)
import Servant.API (JSON, Header, QueryParam, type (:>), type (:<|>)(..), Get)
import Servant.Client (client, mkClientEnv, runClientM, parseBaseUrl, ClientM)

import Config
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

  

