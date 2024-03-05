{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module: TranformTest
-- Description: Test transformations
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
module TransformTest where

import qualified Kipu.Jira.CustomTypes as CT
import Kipu.Time (TimeInterval (..), parseTime)
import Kipu.Transform
import qualified Kipu.Types as T
import qualified Kipu.Utils as U
import BasicPrelude
import qualified Data.Map as M
import Data.Time (UTCTime, addUTCTime, getCurrentTime, nominalDay)
import Test.HUnit

mkChange :: UTCTime -> Text -> Text -> Text -> T.Change
mkChange ts f from to =
  T.Change
    { T.change_timestamp = ts,
      T.change_author = T.User "foo",
      T.change_field = f,
      T.change_type = "jira",
      T.change_from = "",
      T.change_fromString = from,
      T.change_to = "",
      T.change_toString = to
    }

sampleChanges01 :: [T.Change]
sampleChanges01 =
  [ mkChange t0 "status" "To Do" "In Progress",
    mkChange t1 "status" "In Progress" "Blocked",
    mkChange t2 "status" "Blocked" "In Progress",
    mkChange t3 "status" "In Progress" "Done"
  ]
  where
    t0 : t1 : t2 : t3 : _ = expectedTimestamps

statusSequence01 :: [T.Status]
statusSequence01 = map T.Status ["To Do", "In Progress", "Blocked", "In Progress", "Done"]

changes :: IO [T.Change]
changes = do
  is :: Either String [CT.IssueBean] <-
    U.fromFile
      "test/samples/issuebean-changelog.json"
  case is of
    Left s -> assertFailure $ "Error: " ++ show s
    Right [] -> assertFailure "Unexpected empty [IssueBean]"
    Right (x : _) -> case T.getChanges x of
      Nothing -> assertFailure "Error: cannot get changes from issue"
      Just cs -> return cs

time0 :: UTCTime
time0 = addUTCTime (-nominalDay) (head expectedTimestamps)

expectedTimestamps :: [UTCTime]
expectedTimestamps =
  mapMaybe
    parseTime
    [ "2024-01-29T09:04:34.533+0000",
      "2024-01-29T09:04:59.866+0000",
      "2024-01-29T09:08:06.018+0000",
      "2024-01-30T08:46:38.765+0000",
      "2024-01-30T08:46:41.194+0000",
      "2024-01-30T08:46:44.160+0000",
      "2024-01-30T08:46:47.685+0000",
      "2024-02-01T22:22:22.326+0000",
      "2024-02-01T22:23:48.835+0000",
      "2024-02-19T07:45:20.069+0000",
      "2024-02-19T09:07:02.766+0000",
      "2024-02-19T09:07:06.330+0000"
    ]

expectedStates :: M.Map UTCTime T.Status
expectedStates =
  M.fromList
    [ (t0, T.Status "In Progress"),
      (t1, T.Status "Blocked"),
      (t2, T.Status "In Progress"),
      (t3, T.Status "Done")
    ]
  where
    t0 : t1 : t2 : t3 : _ = expectedTimestamps

expectedHistory :: [(UTCTime, T.Status)]
expectedHistory = zip (time0 : expectedTimestamps) statusSequence01

expectedIntervals :: IO [(TimeInterval, T.Status)]
expectedIntervals = do
  t <- getCurrentTime
  return
    $ zip
      ( zipWith
          TimeInterval
          (time0 : expectedTimestamps)
          (expectedTimestamps ++ [t])
      )
      statusSequence01

transformTest :: [Test]
transformTest =
  [ TestLabel
      "Changes timestamps"
      ( TestCase $ do
          cs <- changes
          assertEqual "" (map T.change_timestamp cs) expectedTimestamps
      ),
    TestLabel
      "Status changes"
      ( TestCase $ do
          assertEqual "" (issueStates sampleChanges01) expectedStates
      ),
    TestLabel
      "history"
      ( TestCase $ do
          assertEqual
            ""
            expectedHistory
            $ history
              time0
              (\x -> T.change_field x == "status")
              T.Status
              sampleChanges01
      ),
    TestLabel
      "intervals"
      ( TestCase $ do
          i <- intervals expectedHistory
          e <- expectedIntervals
          assertEqual "" (take 4 e) (take 4 i)
      ),
    TestLabel
      "intervals'"
      ( TestCase $ do
          e <- expectedIntervals
          assertEqual "" (take 4 e) $ intervals' expectedHistory
      )
  ]
