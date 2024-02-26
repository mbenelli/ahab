{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TransformTest where

import qualified Ahab.Jira.CustomTypes as CT
import qualified Ahab.Types as T
import qualified Ahab.Utils as U
import Data.Time (UTCTime)
import BasicPrelude
import Test.HUnit

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

expectedTimeStamps :: [UTCTime]
expectedTimeStamps = mapMaybe T.parseTime
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

transformTest :: [Test]
transformTest =
  [ TestLabel
      "IssueBean"
     ( TestCase $ do
          cs <- changes
          assertEqual "" (map T.change_timestamp cs) expectedTimeStamps
      )
  ]

