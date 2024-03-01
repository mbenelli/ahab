{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ahab.Time where

import BasicPrelude
import Data.Text (unpack)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, zonedTimeToUTC)
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
