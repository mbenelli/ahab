{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module: Ahab.Transformer
-- Description: Data transformations
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
module Ahab.Transform where

import Ahab.Time
import Ahab.Types
import BasicPrelude
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime)

groupByKey :: (Eq k, Hashable k) => (v -> k) -> [v] -> H.HashMap k [v]
groupByKey f = foldr (\x -> H.insertWith (++) (f x) [x]) H.empty

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
intervals :: [(UTCTime, a)] -> IO [(TimeInterval, a)]
intervals ((t0, a0) : (t1, a1) : xs) = do
  ys <- intervals ((t1, a1) : xs)
  return $ (TimeInterval t0 t1, a0) : ys
intervals [(ti, ai)] = do
  t <- getCurrentTime
  return [(TimeInterval ti t, ai)]
intervals [] = return []

-- Get all intervals except the last one, that it the current state.
-- Useful for closed issues
--
intervals' :: [(UTCTime, a)] -> [(TimeInterval, a)]
intervals' ((t0, a0) : (t1, a1) : xs) =
  (TimeInterval t0 t1, a0) : intervals' ((t1, a1) : xs)
intervals' [(_, _)] = []
intervals' [] = []

states :: (Issue a) => a -> Maybe (M.Map TimeInterval Status)
states i = do
  cs <- changelog i
  return
    $ M.fromList
    $ intervals'
    $ history
      (created i)
      (\x -> change_field x == "status")
      Status
      cs

cumulativeStatusTime :: (Issue a) => a -> Status -> Maybe NominalDiffTime
cumulativeStatusTime i s =
  states i
    >>= Just
    . M.foldrWithKey
      ( \k v t -> if v == s then t + duration k else t
      )
      0

assignees :: (Issue a) => a -> Maybe (S.Set User)
assignees i = do
  cs <- changelog i
  u <- assignee i
  return
    $ S.fromList
    $ u
    : [pseudononimize $ change_toString x | x <- cs, change_field x == "assignee"]
