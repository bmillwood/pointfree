module Pointfree where

import Plugin.Pl.Common (mapTopLevel, mapTopLevel')
import Plugin.Pl.Optimize (optimize)
import Plugin.Pl.Parser (parsePF)
import Plugin.Pl.PrettyPrinter (prettyTopLevel)
import Plugin.Pl.Transform (transform)

import Data.Maybe (listToMaybe)

{- |
  >>> pointfree "I'm not a valid Haskell expression!"
  []
  >>> pointfree "sum xs = foldr (+) 0 xs"
  ["sum = id (fix (const (foldr (+) 0)))","sum = fix (const (foldr (+) 0))","sum = foldr (+) 0"]
-}
pointfree :: String -> [String]
pointfree
  = either
    (const [])
    (map prettyTopLevel . mapTopLevel' optimize . mapTopLevel transform)
  . parsePF

{- |
  >>> pointfree' "I'm not a valid Haskell expression!"
  Nothing
  >>> pointfree' "sum xs = foldr (+) 0 xs"
  Just "sum = foldr (+) 0"
-}
pointfree' :: String -> Maybe String
pointfree' = listToMaybe . reverse . pointfree
