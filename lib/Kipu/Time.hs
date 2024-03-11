{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module: Kipu.Time
-- Description: Time Utilities
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
module Kipu.Time where

import BasicPrelude hiding (union)
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

contains :: TimeInterval -> UTCTime -> Bool
contains ti t = (begin ti <= t) && (t < end ti)

contains' :: TimeInterval -> UTCTime -> Bool
contains' ti t = (begin ti <= t) && (t <= end ti)

intersection :: TimeInterval -> TimeInterval -> Maybe TimeInterval
intersection x y
  | begin x <= begin y && end y <= end x = Just y
  | begin y <= begin x && end x <= end y = Just x
  | begin x == begin y && end x <= end y = Just x
  | begin x == begin y && end y <= end x = Just y
  | begin x <= begin y && end x == end y = Just y
  | begin y <= begin x && end x == end y = Just x
  | begin x <= begin y && begin y < end x && end x <= end y =
      Just $ timeInterval (begin y) (end x)
  | begin y <= begin x && begin x < end y && end y <= end x =
      Just $ timeInterval (begin x) (end y)
  | otherwise = Nothing

intersection' :: [TimeInterval] -> [TimeInterval] -> [TimeInterval]
intersection' (x : xs) ys = mapMaybe (intersection x) ys ++ intersection' xs ys
intersection' [] _ = []

union :: TimeInterval -> TimeInterval -> [TimeInterval]
union x y
  | begin x < begin y && end y < end x = [x]
  | begin y < begin x && end x < end y = [y]
  | begin x < begin y && end x < end y = [timeInterval (begin x) (end y)]
  | begin y < begin x && end y < end x = [timeInterval (begin y) (end x)]
  | otherwise = [x, y]

union' :: [TimeInterval] -> [TimeInterval]
union' (x0 : x1 : xs) =
  let u = (union x0 x1)
   in case length u of
        1 -> union' $ u ++ xs
        _ -> union' $ (last u) : xs
union' [] = []
union' (x : []) = [x]

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

chooseInterval :: [TimeInterval] -> UTCTime -> Maybe TimeInterval
chooseInterval tis t = find (\i -> contains' i t) tis

-- | Partition a list by an interval set.
partitionByIntervals :: [TimeInterval] -> (a -> UTCTime) -> [a] -> M.Map TimeInterval [a]
partitionByIntervals is t =
  foldl'
    ( \m x ->
        case chooseInterval is (t x) of
          Nothing -> m
          Just i -> M.insertWith (++) i [x] m
    )
    M.empty
