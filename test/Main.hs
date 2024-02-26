{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import BasicPrelude
import ConfigTest (configTest)
import IssueBeanTest (issueBeanTest)
import Test.HUnit

main :: IO ()
main = do
  runTestTTAndExit $ TestList $ configTest ++ issueBeanTest
