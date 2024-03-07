{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module: TimeTest
-- Description: Test transformations
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
module TimeTest where

import BasicPrelude
import qualified Data.Map as M
import Data.Time (UTCTime, addUTCTime, getCurrentTime, nominalDay)
import Kipu.Time
import Test.HUnit

sampleTimestamps :: [UTCTime]
sampleTimestamps =
  mapMaybe
    parseTime
    [ "2024-01-29T01:00:00.000+0000",
      "2024-01-29T02:00:00.000+0000",
      "2024-01-29T03:00:00.000+0000",
      "2024-01-30T04:00:00.000+0000",
      "2024-01-30T05:00:00.000+0000",
      "2024-01-30T06:00:00.000+0000",
      "2024-01-30T07:00:00.000+0000",
      "2024-02-01T01:00:00.000+0000",
      "2024-02-01T02:00:00.000+0000",
      "2024-02-19T03:00:00.000+0000",
      "2024-02-19T04:00:00.000+0000",
      "2024-02-19T05:00:00.000+0000"
    ]
