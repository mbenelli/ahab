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
          --            let j = B.readFile "test/samples/issuebean.json"
          --            i <- (eitherDecode <$> j) :: IO (Either String T.IssueBean)
          i :: Either String CT.IssueBean <- U.fromFile "test/samples/issuebean.json"
          case i of
            Left s -> assertFailure $ "Error: " ++ show s
            Right x -> assertEqual "" (CT.issueBean_key x) "OR-3"
      ),
    TestLabel
      "Beans"
      ( TestCase $ do
          --            let j = B.readFile "test/samples/issuebeans.json"
          --            is <- (eitherDecode <$> j) :: IO (Either String [T.IssueBean])
          is :: Either String [CT.IssueBean] <- U.fromFile "test/samples/issuebeans.json"
          case is of
            Left s -> assertFailure $ "Error: " ++ show s
            Right x -> assertEqual "" (length x) 3
      )
  ]
