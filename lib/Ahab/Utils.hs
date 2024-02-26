{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ahab.Utils where

import Ahab.Client (run)
import Ahab.Config
import Ahab.Jira.Api
import Ahab.Jira.CustomTypes
import qualified Ahab.Jira.Types as JT
import Ahab.Types
import BasicPrelude
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as BL
import Data.HashMap.Strict as M (fromList, lookup, toList)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as I
import Servant.Client (ClientM)
import System.IO (IOMode (WriteMode), openFile)
import Text.Pretty.Simple
import Text.Printf

-- Save and load from file
--

fromFile :: (FromJSON a) => Text -> IO (Either String a)
fromFile f = eitherDecode <$> BL.readFile (T.unpack f)

toFile :: (ToJSON a) => a -> Text -> IO ()
toFile i f = I.writeFile (T.unpack f) (encodeToLazyText i)

-- Fiels related information
--

allFields :: IO (Either Text (HashMap Text Text))
allFields = do
  r <- run fieldsQuery
  case r of
    Left e -> return (Left e)
    Right fs -> return $ Right $ fromList $ map getNameId fs
  where
    getNameId x = (JT.fieldDetails_name x, JT.fieldDetails_id x)

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
--

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

withFields :: (Show a) => ([JT.FieldDetails] -> a) -> IO ()
withFields = withContinuation fieldsQuery

withIssueTypes :: (Show a) => ([JT.IssueTypeDetails] -> a) -> IO ()
withIssueTypes = withContinuation issueTypeQuery

withIssue :: (Show a) => Text -> (IssueBean -> a) -> IO ()
withIssue x = withContinuation $ issueQuery x

withChangelog :: (Show a) => Text -> (JT.PageBeanChangelog -> a) -> IO ()
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

optimisticLoadIssue :: Text -> IO (IssueBean, [Change])
optimisticLoadIssue f = do
  eibs :: Either String [IssueBean] <- fromFile f
  let Right ibs = eibs
  let ib :: IssueBean = head ibs
  let cs :: [Change] = fromJust $ getChanges ib
  return (ib, cs)
