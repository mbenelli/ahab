{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Ghidorah.Field where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict as M
import Data.Text
import GHC.Generics


data Field = Field
  { fieldId :: !Text
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
  { field_clausesNames :: !(Maybe [Text])
  , field_custom :: !(Maybe Bool)
  , field_id :: !(Maybe Text)
  , field_key :: !(Maybe Text)
  , field_name :: !(Maybe Text)
  , field_navigable :: !(Maybe Bool)
  , field_orderable :: !(Maybe Bool)
  , field_searchable :: !(Maybe Bool)
  }
  deriving (Show, Generic)

instance FromJSON FieldDetails where
  parseJSON = genericParseJSON defaultOptions{ fieldLabelModifier = drop 6 }

