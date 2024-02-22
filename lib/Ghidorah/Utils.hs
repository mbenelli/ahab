{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ghidorah.Utils where

import BasicPrelude
import Data.HashMap.Strict as M (fromList, lookup, toList)
import Ghidorah.Client (run)
import Ghidorah.Config
import Ghidorah.Jira.Api
import Ghidorah.Jira.CustomTypes
import Ghidorah.Jira.Types
import Servant.Client (ClientM)
import System.IO (IOMode (WriteMode), openFile)
import Text.Pretty.Simple
import Text.Printf

allFields :: IO (Either Text (HashMap Text Text))
allFields = do
  r <- run fieldsQuery
  case r of
    Left e -> return (Left e)
    Right fs -> return $ Right $ fromList $ map getNameId fs
  where
    getNameId x = (fieldDetails_name x, fieldDetails_id x)

--      getNameId x = case (fieldDetails_name x, fieldDetails_id x) of
--        (Just n, Just i) -> Just (n, i)
--        (_, _) ->  Nothing

searchField :: Text -> IO Text
searchField f = do
  fs <- allFields
  case fs of
    Left e -> return e
    Right xs -> case M.lookup f xs of
      Just r -> return r
      Nothing -> return "Not found"

printFields :: Maybe FilePath -> IO ()
printFields path = do
  handle <- openFile (fromMaybe "/dev/stdout" path) WriteMode
  fs <- allFields
  case fs of
    Left e -> printf "%s" e
    Right x -> mapM_ (prt handle) (toList x)
  where
    prt h (n, i) = hPrintf h "%-30s %s\n" n i

-- Jira most useful fields
--

defaultFields :: [Text]
defaultFields =
  [ "id",
    "key",
    "project",
    "issuetype",
    "summary",
    "status",
    "created",
    "creator",
    "priority",
    "description",
    "assignee",
    "reporter",
    "fixVersions",
    "versions",
    "components",
    "issuelink",
    "resolution",
    "resolutiondate"
  ]

-- Printers

withContinuation :: (Show b) => (Config -> ClientM a) -> (a -> b) -> IO ()
withContinuation f k = do
  res <- run f
  case res of
    Left l -> putStrLn l
    Right r -> pPrintNoColor $ k r

withSearchResult ::
  (Show a) =>
  Text ->
  Int ->
  [Text] ->
  (SearchResponse -> a) ->
  IO ()
withSearchResult q i fs = withContinuation $ searchQuery q i fs

withFields :: (Show a) => ([FieldDetails] -> a) -> IO ()
withFields = withContinuation fieldsQuery

withIssueTypes :: (Show a) => ([IssueTypeDetails] -> a) -> IO ()
withIssueTypes = withContinuation issueTypeQuery

withIssue :: (Show a) => Text -> (IssueBean -> a) -> IO ()
withIssue x = withContinuation $ issueQuery x

withChangelog :: (Show a) => Text -> (PageBeanChangelog -> a) -> IO ()
withChangelog x = withContinuation $ changelogQuery x

createIssue :: CreateIssueRequest -> IO ()
createIssue x = withContinuation (createIssue' x) BasicPrelude.id

collectSearchResult ::
  Text ->
  Int ->
  [IssueBean] ->
  IO (Either Text [IssueBean])
collectSearchResult q i xs = do
  res <- run $ searchQuery q i defaultFields
  case res of
    Left e -> return $ Left e
    Right r ->
      if BasicPrelude.null (issues r)
        then return $ Right xs
        else
          collectSearchResult
            q
            (BasicPrelude.length (xs ++ issues r))
            (xs ++ issues r)

collectSearchResult' :: Text -> Int -> Int -> [IssueBean] -> IO (Either Text [IssueBean])
collectSearchResult' q i j xs = do
  res <- run $ searchQuery q i defaultFields
  case res of
    Left e -> return $ Left e
    Right r ->
      if BasicPrelude.length xs >= j
        then return $ Right xs
        else collectSearchResult' q (BasicPrelude.length (xs ++ (issues r))) j (xs ++ (issues r))
