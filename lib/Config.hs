{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import GHC.Generics
import Data.Yaml
import System.Directory

data Config = Config
  { crtPath :: !String
  , keyPath :: !String
  , url :: !String
  , token :: !String
  } deriving (Eq, Show, Generic)

instance FromJSON Config

defaultConfigFile ::  IO String
defaultConfigFile = getXdgDirectory XdgConfig "ghidorah/config.yaml"


readConfig :: String -> IO (Either ParseException Config)
readConfig = decodeFileEither 

--main :: IO ()
--main = do
--  c <- readConfig "config.yaml"
--  case c of
--    Left err -> putStrLn $ "Error: " ++ show err
--    Right conf -> print conf
