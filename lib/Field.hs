{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module Field where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict as M
import GHC.Generics


data Field = Field
  { fieldId :: !String
  , fieldName :: !(Maybe String)
  , fieldValue :: !(Maybe String)
  }
  deriving Show

instance {-# OVERLAPPING #-} FromJSON [Field] where
  parseJSON x =
    parseJSON x >>= mapM parseField . toList . M.filter (\case
      Object _ -> True
      _  -> False)

parseField :: (String, Value) -> Parser Field
parseField (i, v) =
  withObject "field body" (\ o ->
    Field i <$> o .:? "name" <*> o .:? "value")
    v

data FieldDetails = FieldDetails
  { clausesNames :: !(Maybe [String])
  , custom :: !(Maybe Bool)
  , id :: !(Maybe String)
  , key :: !(Maybe String)
  , name :: !(Maybe String)
  , navigable :: !(Maybe Bool)
  , orderable :: !(Maybe Bool)
  , searchable :: !(Maybe Bool)
  }
  deriving (Show, Generic)

instance FromJSON FieldDetails

