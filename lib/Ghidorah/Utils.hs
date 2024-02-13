{-# LANGUAGE OverloadedStrings #-}

module Ghidorah.Utils where

import Prelude (Either(..), IO, return, ($), show)

import Data.HashMap.Strict (HashMap, fromList, lookup)
import Data.Maybe (mapMaybe, Maybe(..))
import Data.Text (Text, pack)

import Ghidorah.Client (run, fieldsQuery)
import Ghidorah.Types (field_name, field_id)

fields :: IO (Either Text (HashMap Text Text))
fields = do
  r <- run fieldsQuery
  case r of
    Right fs -> return $ Right $ fromList $ mapMaybe getNameId fs
      where
       getNameId x = case (field_name x, field_id x) of 
        (Just n, Just i) -> Just (n, i)
        (_, _) ->  Nothing
    Left e -> return (Left e)

searchField :: Text -> IO Text
searchField f = do
   fs <- fields
   case fs of
     Left e -> return $ pack $ show e
     Right xs -> case lookup f xs of
       Just r -> return r
       Nothing -> return "Not found"

