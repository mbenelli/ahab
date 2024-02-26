{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ahab.Transform where

import Ahab.Types
import BasicPrelude
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time (UTCTime)

issueStates :: [Change] -> M.Map UTCTime Status
issueStates cs =
  M.fromList
    [ (change_timestamp x, Status $ change_toString x)
      | x <- cs,
        change_field x == "status"
    ]

-- FIXME :: this implementation do not consider the last staus chanege,
-- and interpret incorrectly the first one
--
statusIntervals :: Status -> [Change] -> M.Map UTCTime UTCTime
statusIntervals s cs = M.fromList $ zip to from
  where
    changes = filter (\x -> change_field x == "status") cs
    to = [change_timestamp x | x <- changes, Status (change_toString x) == s]
    from = [change_timestamp x | x <- changes, Status (change_fromString x) == s]

assignees :: [Change] -> S.Set User
assignees cs = S.fromList [User $ change_toString x | x <- cs, change_field x == "assignee"]
