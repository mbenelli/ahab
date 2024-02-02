{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module Field where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict as M


data Field = Field
  { fieldId :: !String
  , name :: !(Maybe String)
  , value :: !(Maybe String)
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

