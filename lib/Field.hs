{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Field where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict as M
import Data.Text
import GHC.Generics


data Field = Field
  { fieldId :: Text
  , fieldName :: !(Maybe Text)
  , fieldValue :: !(Maybe Text)
  }
  deriving Show

instance {-# OVERLAPPING #-} FromJSON [Field] where
  parseJSON x =
    parseJSON x >>= mapM parseField . toList . M.filter (\case
      Object _ -> True
      _  -> False)

parseField :: (Text, Value) -> Parser Field
parseField (i, v) =
  withObject "field body" (\ o ->
    Field i <$> o .:? "name" <*> o .:? "value")
    v

data FieldDetails = FieldDetails
  { clausesNames :: !(Maybe [Text])
  , custom :: !(Maybe Bool)
  , id :: !(Maybe Text)
  , key :: !(Maybe Text)
  , name :: !(Maybe Text)
  , navigable :: !(Maybe Bool)
  , orderable :: !(Maybe Bool)
  , searchable :: !(Maybe Bool)
  }
  deriving (Show, Generic)

instance FromJSON FieldDetails

