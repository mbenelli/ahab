{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Ghidorah.Config where

import Data.Text
import Data.Yaml
import GHC.Generics
import System.Directory

data Config = Config
  { url :: !Text
  , user :: !Text
  , authorization :: !Text
  , token :: !Text
  , crtPath :: !(Maybe Text)
  , keyPath :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance FromJSON Config

defaultConfigFile ::  IO String
defaultConfigFile = getXdgDirectory XdgConfig "ghidorah/config.yaml"


readConfig :: String -> IO (Either ParseException Config)
readConfig = decodeFileEither 

