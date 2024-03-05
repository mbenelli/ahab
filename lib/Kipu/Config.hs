{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Kipu.Config
-- Description: Configuration
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
module Kipu.Config where

import Data.Text as T
import Data.Yaml
import GHC.Generics
import System.Directory

data Config = Config
  { url :: !Text,
    user :: !Text,
    authorization :: !Text,
    token :: !Text,
    crtPath :: !(Maybe Text),
    keyPath :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

instance FromJSON Config

auth :: Config -> Text
auth c = T.unwords [authorization c, token c]

defaultConfigFile :: IO String
defaultConfigFile = getXdgDirectory XdgConfig "ahab/config.yaml"

readConfig :: String -> IO (Either ParseException Config)
readConfig = decodeFileEither
