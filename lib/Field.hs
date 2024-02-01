{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Field where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict


data Field = Field
  { fieldId :: !String
  , name :: !(Maybe String)
  , value :: !(Maybe String)
  }
  deriving Show

instance {-# OVERLAPPING #-} FromJSON [Field] where
  parseJSON x =
    parseJSON x >>= mapM parseField . toList

parseField :: (String, Value) -> Parser Field
parseField (i, v) =
  withObject "field body" (\ o ->
    Field i <$> o .:? "name" <*> o .:? "location")
    v

