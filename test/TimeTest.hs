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
import Data.Time (UTCTime)
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

timeTest :: [Test]
timeTest =
  [ TestLabel
      "TimeInterval creation"
      ( TestCase $ do
          let a = sampleTimestamps !! 0
          let b = sampleTimestamps !! 1
          let dt0 = timeInterval a b
          let dt1 = timeInterval b a
          assertEqual "" dt0 dt1
          assertEqual "" (begin dt0) a
          assertEqual "" (begin dt1) a
          assertEqual "" (end dt0) b
          assertEqual "" (end dt1) b
      ),
    TestLabel
      "Intersection"
      ( TestCase $ do
          let t0 : t1 : t2 : t3 : _ = sampleTimestamps
          let i1 = timeInterval t0 t2
          let i2 = timeInterval t1 t3
          let int = timeInterval t1 t2
          case intersection i1 i2 of
            Nothing -> assertFailure "Intersection should not be empty"
            Just ii -> assertEqual "" ii int
      ),
    TestLabel
      "Intersections"
      ( TestCase $ do
          let t0 : t1 : t2 : _ : _ : t5 : t6 : t7 : t8 : _ = sampleTimestamps
          let i0 = timeInterval t0 t1
          let i1 = timeInterval t6 t8
          let i2 = timeInterval t0 t2
          let i3 = timeInterval t5 t7
          let xs = [i0, i1]
          let ys = [i2, i3]
          let ii = intersection' xs ys
          let ii0 = timeInterval t0 t1
          let ii1 = timeInterval t6 t7
          assertEqual "" ii [ii0, ii1]
      )
  ]
