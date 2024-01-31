module Main (main) where

import Config

main :: IO ()
main = do
  c <- readConfig "config.yaml.tmpl"
  case c of
    Left err -> putStrLn $ "Error: " ++ show err
    Right conf -> print conf
