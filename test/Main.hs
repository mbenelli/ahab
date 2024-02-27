-- |
-- Module: Main
-- Description: Test suite
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com 
--

{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import BasicPrelude
import ConfigTest (configTest)
import IssueBeanTest (issueBeanTest)
import Test.HUnit
import TransformTest (transformTest)

main :: IO ()
main = do
  runTestTTAndExit $ TestList $ configTest ++ issueBeanTest ++ transformTest
