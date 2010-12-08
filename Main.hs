module Main where

import Plugin.Pl.Common
import Plugin.Pl.Optimize
import Plugin.Pl.Parser
import Plugin.Pl.Transform

import System.Environment (getArgs)
import System.Console.GetOpt

data Flag = Verbose 
  deriving Eq

options :: [OptDescr Flag]
options = [ Option ['v'] ["verbose"] (NoArg Verbose) "verbose results"]

header :: String
header = "Usage: pointfree [OPTION...] query"

parseArgs :: [String] -> IO ([Flag], [String])
parseArgs args =
  case getOpt Permute options args of
    (flags, nonOptions, []) -> return (flags, nonOptions)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))

main :: IO ()
main = do
  args <- getArgs
  (flags, nonOptions) <- parseArgs args
  if null nonOptions
     then putStrLn $ usageInfo header options
     else let query = concat $ intersperse " " nonOptions
              verbose = Verbose `elem` flags
          in pf query verbose

pf :: String -> Bool -> IO ()
pf input verbose = case parsePF input of
  Right d ->
    if verbose
       then do putStrLn "Transformed to pointfree style:"
               let d' = mapTopLevel transform d
               print $ d'
               putStrLn "Optimized expression:"
               mapM_ print $ mapTopLevel' optimize d'
       else print $ last $ mapTopLevel' optimize $ mapTopLevel transform d
  Left err -> putStrLn err

