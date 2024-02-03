{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import GHC.Generics
import Data.Yaml
import System.Directory

data Config = Config
  { url :: !String
  , user :: !String
  , authorization :: !String
  , token :: !String
  , crtPath :: !(Maybe String)
  , keyPath :: !(Maybe String)
  } deriving (Eq, Show, Generic)

instance FromJSON Config

defaultConfigFile ::  IO String
defaultConfigFile = getXdgDirectory XdgConfig "ghidorah/config.yaml"


readConfig :: String -> IO (Either ParseException Config)
readConfig = decodeFileEither 

