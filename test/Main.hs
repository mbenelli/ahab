{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy as B
import Data.Text
import Ahab.Config
import Ahab.Jira.CustomTypes as JT
import Test.HUnit

sampleConfig :: Config
sampleConfig =
  Config
    { url = "https://sample.url.com",
      user = "ghidorah",
      authorization = "Bearer",
      token = "abcdefghijklmnopqrstuvwxyz1234567890",
      crtPath = Just "path_to_certificate.crt",
      keyPath = Just "path_to_key.key"
    }

tests :: Test
tests =
  TestList
    [ TestLabel
        "Config"
        ( TestCase $ do
            c <- readConfig "test/samples/config/config.yaml"
            case c of
              Left err -> assertFailure $ "Error: " ++ show err
              Right conf -> assertEqual "Sample Config" conf sampleConfig
        ),
      TestLabel
        "Config Without Certificates"
        ( TestCase $ do
            c <- readConfig "test/samples/config/config-nocerts.yaml"
            case c of
              Left err -> assertFailure $ "Error: " ++ show err
              Right conf -> assertEqual "" (url conf) (url sampleConfig)
        ),
      TestLabel
        "Types"
        ( TestCase $ do
            let j = B.readFile "test/samples/issue.json"
            i <- (eitherDecode <$> j) :: IO (Either String JT.IssueBean)
            case i of
              Left s -> assertFailure $ "Error: " ++ show s
              Right x -> assertEqual "" (JT.issue_key x) "TP-1"
        )
    ]

main :: IO ()
main = runTestTTAndExit tests
