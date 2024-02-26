{-# LANGUAGE OverloadedStrings #-}

module ConfigTest where

import Ahab.Config
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

configTest :: [Test]
configTest =
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
      )
  ]
