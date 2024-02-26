{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IssueBeanTest where

import qualified Ahab.Jira.CustomTypes as CT
import qualified Ahab.Utils as U
import Test.HUnit

issueBeanTest :: [Test]
issueBeanTest =
  [ TestLabel
      "IssueBean"
      ( TestCase $ do
          i :: Either String CT.IssueBean <- U.fromFile "test/samples/issuebean.json"
          case i of
            Left s -> assertFailure $ "Error: " ++ show s
            Right x -> assertEqual "" (CT.issueBean_key x) "OR-3"
      ),
    TestLabel
      "Beans"
      ( TestCase $ do
          is :: Either String [CT.IssueBean] <- U.fromFile "test/samples/issuebeans.json"
          case is of
            Left s -> assertFailure $ "Error: " ++ show s
            Right x -> assertEqual "" (length x) 3
      )
  ]
