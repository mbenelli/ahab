{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TransformTest where

import qualified Ahab.Jira.CustomTypes as CT
import qualified Ahab.Types as T
import qualified Ahab.Utils as U
import BasicPrelude
import Test.HUnit

transformTest :: [Test]
transformTest =
  [ TestLabel
      "IssueBean"
      ( TestCase $ do
          is :: Either String [CT.IssueBean] <- U.fromFile "test/samples/issuebean-changelog.json"
          case is of
            Left s -> assertFailure $ "Error: " ++ show s
            Right x -> case T.getChanges $ head x of
              Nothing -> assertFailure $ "Error: cannot get changes from issue"
              Just cs -> assertEqual "" (length cs) 12
      )
  ]
