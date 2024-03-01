{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ahab.Time where

import BasicPrelude
import Data.Text (unpack)
import Data.Time (DayOfWeek (Saturday, Sunday), NominalDiffTime, UTCTime, dayOfWeek, diffUTCTime, utctDay, zonedTimeToUTC)
import Data.Time.Format (defaultTimeLocale, parseTimeM)

data TimeInterval = TimeInterval !UTCTime !UTCTime
  deriving (Show, Eq)

instance Ord TimeInterval where
  (<) a b = begin a < begin b
  (<=) a b = a == b || a < b

begin :: TimeInterval -> UTCTime
begin (TimeInterval b _) = b

end :: TimeInterval -> UTCTime
end (TimeInterval _ e) = e

duration :: TimeInterval -> NominalDiffTime
duration (TimeInterval b e) = diffUTCTime e b

parseTime :: Text -> Maybe UTCTime
parseTime s = do
  t <- parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" $ unpack s
  return $ zonedTimeToUTC t

-- | Return the working days included in the interval.
--
-- Weekends are excluded, beginning and end days are included in the count.
-- Remarks:
-- Timestamp are in UTC time zone.
-- Holidays are not considered are they are not known.
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
