{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ahab.Transform where

import Ahab.Types
import BasicPrelude
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time (UTCTime, getCurrentTime)

issueStates :: [Change] -> M.Map UTCTime Status
issueStates cs =
  M.fromList
    [ (change_timestamp x, Status $ change_toString x)
      | x <- cs,
        change_field x == "status"
    ]

history :: UTCTime -> (Change -> Bool) -> (Text -> a) -> [Change] -> [(UTCTime, a)]
history _ _ _ [] = []
history t0 f ctor (c : cs) =
  (t0, ctor $ change_fromString c)
    : [(change_timestamp x, ctor $ change_toString x) | x <- c : cs, f x]

-- Get all intervals, the last one is from the timestamp the issue went
-- to its current state to now. Since it gets the current time
-- it returns an IO
--
intervals :: [(UTCTime, a)] -> IO [(UTCTime, UTCTime, a)]
intervals ((t0, a0) : (t1, a1) : xs) = do
  ys <- intervals ((t1, a1) : xs)
  return $ (t0, t1, a0) : ys
intervals [(ti, ai)] = do
  t <- getCurrentTime
  return [(ti, t, ai)]
intervals [] = return []

-- Get all intervals except the last one, that it the current state.
-- Useful for closed issues
--
intervals' :: [(UTCTime, a)] -> [(UTCTime, UTCTime, a)]
intervals' ((t0, a0) : (t1, a1) : xs) = (t0, t1, a0) : intervals' ((t1, a1) : xs)
intervals' [(_, _)] = []
intervals' [] = []

assignees :: [Change] -> S.Set User
assignees cs = S.fromList [User $ change_toString x | x <- cs, change_field x == "assignee"]
