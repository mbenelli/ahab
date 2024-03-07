{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module: Main
-- Description: Test suite
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
module Main (main) where

import BasicPrelude
import ConfigTest (configTest)
import IssueBeanTest (issueBeanTest)
import Test.HUnit
import TimeTest (timeTest)
import TransformTest (transformTest)

main :: IO ()
main = do
  runTestTTAndExit
    $ TestList
    $ configTest
    ++ issueBeanTest
    ++ transformTest
    ++ timeTest
