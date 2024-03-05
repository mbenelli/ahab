{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module: Kipu.Time
-- Description: Time Utilities
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
module Kipu.Time where

import BasicPrelude
import qualified Data.Map as M
import Data.Text (unpack)
import Data.Time
  ( DayOfWeek (Saturday, Sunday),
    NominalDiffTime,
    UTCTime,
    dayOfWeek,
    diffUTCTime,
    utctDay,
    zonedTimeToUTC,
  )
import Data.Time.Calendar (DayOfWeek (Monday), Year)
import Data.Time.Calendar.WeekDate (FirstWeekType (..), WeekOfYear, toWeekCalendar)
import Data.Time.Format (defaultTimeLocale, parseTimeM)

-- | Parse ISO 8601 time
--
-- Do not use `iso8601ParseM` because it use different time zone
-- notation.
parseTime :: Text -> Maybe UTCTime
parseTime s = do
  t <- parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" $ unpack s
  return $ zonedTimeToUTC t

data TimeInterval = TimeInterval !UTCTime !UTCTime
  deriving (Show, Eq)

timeInterval :: UTCTime -> UTCTime -> TimeInterval
timeInterval a b = case compare a b of
  LT -> TimeInterval a b
  EQ -> TimeInterval a b
  GT -> TimeInterval b a

instance Ord TimeInterval where
  (<) a b = begin a < begin b
  (<=) a b = a == b || a < b

begin :: TimeInterval -> UTCTime
begin (TimeInterval b _) = b

end :: TimeInterval -> UTCTime
end (TimeInterval _ e) = e

duration :: TimeInterval -> NominalDiffTime
duration (TimeInterval b e) = diffUTCTime e b

-- | Return the working days included in the interval.
--
-- Weekends are excluded, beginning and end days are included in the count.
-- Remarks:
-- Timestamp are in UTC time zone.
-- Holidays are not considered as they are not known.
workingDays :: TimeInterval -> Int
workingDays i =
  foldl'
    ( \n x ->
        if dayOfWeek x == Saturday || dayOfWeek x == Sunday then n else n + 1
    )
    0
    [b .. e]
  where
    b = utctDay $ begin i
    e = utctDay $ end i

-- | Partition a list by week
-- Given a fuction that maps each element of the list to a date,
-- returns the elements grouped by week.
partitionByWeek :: (a -> UTCTime) -> [a] -> M.Map (Year, WeekOfYear) [a]
partitionByWeek t =
  foldl'
    ( \m x ->
        let (y, w, _) = toWeekCalendar FirstWholeWeek Monday $ utctDay $ t x
         in M.insertWith (++) (y, w) [x] m
    )
    M.empty
