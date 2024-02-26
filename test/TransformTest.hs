{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TransformTest where

import qualified Ahab.Jira.CustomTypes as CT
import Ahab.Transform
import qualified Ahab.Types as T
import qualified Ahab.Utils as U
import qualified Data.Map as M
import Data.Time (UTCTime)
import BasicPrelude
import Test.HUnit

mkChange :: UTCTime -> Text -> Text -> Text -> T.Change
mkChange ts f from to = T.Change
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
    mkChange t3 "status" "In Progress" "Done" ]
  where
    t0:t1:t2:t3:_ = expectedTimestamps

changes :: IO [T.Change]
changes = do
  is :: Either String [CT.IssueBean] <- U.fromFile
    "test/samples/issuebean-changelog.json"
  case is of
    Left s -> assertFailure $ "Error: " ++ show s
    Right [] -> assertFailure "Unexpected empty [IssueBean]"
    Right (x:_) -> case T.getChanges x of
      Nothing -> assertFailure "Error: cannot get changes from issue"
      Just cs -> return cs

expectedTimestamps :: [UTCTime]
expectedTimestamps = mapMaybe T.parseTime
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
expectedStates = M.fromList
  [(t0, T.Status "In Progress"),
   (t1, T.Status "Blocked"),
   (t2, T.Status "In Progress"),
   (t3, T.Status "Done")]
  where
    t0:t1:t2:t3:_ = expectedTimestamps

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
      )

  ]

