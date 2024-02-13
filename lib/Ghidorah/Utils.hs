{-# LANGUAGE OverloadedStrings #-}

module Ghidorah.Utils where

import Prelude (Either(..), IO, return, ($), mapM_)

import Data.HashMap.Strict (HashMap, fromList, lookup, toList)
import Data.Maybe (mapMaybe, Maybe(..), fromMaybe)
import Data.Text (Text)
import System.IO (FilePath, openFile, IOMode(WriteMode))
import Text.Printf

import Ghidorah.Client (run, fieldsQuery)
import Ghidorah.Types (field_name, field_id)

fields :: IO (Either Text (HashMap Text Text))
fields = do
  r <- run fieldsQuery
  case r of
    Left e -> return (Left e)
    Right fs -> return $ Right $ fromList $ mapMaybe getNameId fs
    where
      getNameId x = case (field_name x, field_id x) of 
        (Just n, Just i) -> Just (n, i)
        (_, _) ->  Nothing

searchField :: Text -> IO Text
searchField f = do
   fs <- fields
   case fs of
     Left e -> return e
     Right xs -> case lookup f xs of
       Just r -> return r
       Nothing -> return "Not found"

printFields :: Maybe FilePath -> IO ()
printFields path = do
  handle <- openFile (fromMaybe "/dev/stdout" path) WriteMode
  fs <- fields
  case fs of
    Left e -> printf "%s" e
    Right x -> mapM_ (print handle) (toList x)
    where
      print h (n, i) = hPrintf h "%-30s %s\n" n i
