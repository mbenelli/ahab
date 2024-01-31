module Main (main) where

import Config

--import Test.Tasty
--import Test.Tasty.HUnit
import Test.HUnit

sampleConfig :: Config
sampleConfig = Config
  { crtPath = "path_to_certificate.crt"
  , keyPath = "path_to_key.key"
  , url = "https://sample.url.com"
  , token = "abcdefghijklmnopqrstuvwxyz1234567890"
  }

tests :: Test
tests = TestList
 [ TestLabel "Config"
     (
     TestCase $ do
       c <- readConfig "doc/config.yaml.tmpl"
       case c of
         Left err -> assertFailure $ "Error: " ++ show err
         Right conf -> assertEqual "Sample Config" conf sampleConfig
     )
 ]

main :: IO ()
main = runTestTTAndExit tests
