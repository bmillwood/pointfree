module Main where

import Plugin.Pl.Common
import Plugin.Pl.Optimize
import Plugin.Pl.Parser
import Plugin.Pl.PrettyPrinter
import Plugin.Pl.Transform

import System.Environment (getArgs)
import System.Console.GetOpt

data Flag = Verbose 
          | StdIn
  deriving Eq

options :: [OptDescr Flag]
options = [ Option ['v'] ["verbose"] (NoArg Verbose) "verbose results"
          , Option []    ["stdin"]   (NoArg StdIn)   "read from stdin"
          ]

header :: String
header = "Usage: pointfree [OPTION...] query"

parseArgs :: [String] -> IO ([Flag], [String])
parseArgs args =
  case getOpt Permute options args of
    (flags, nonOptions, []) -> return (flags, nonOptions)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))

getQuery :: [Flag] -> [String] -> IO String
getQuery flags nonOptions
  | StdIn `elem` flags = getLine
  | otherwise          = return $ unwords nonOptions

main :: IO ()
main = do
  args <- getArgs
  (flags, nonOptions) <- parseArgs args
  query <- getQuery flags nonOptions
  if null query
     then putStrLn $ usageInfo header options
     else let verbose = Verbose `elem` flags
          in pf query verbose

pf :: String -> Bool -> IO ()
pf input verbose = case parsePF input of
  Right d ->
    if verbose
       then do putStrLn "Transformed to pointfree style:"
               let d' = mapTopLevel transform d
               putStrLn $ prettyTopLevel d'
               putStrLn "Optimized expression:"
               mapM_ (putStrLn . prettyTopLevel) $ mapTopLevel' optimize d'
       else putStrLn . prettyTopLevel . last . mapTopLevel' optimize $ mapTopLevel transform d
  Left err -> putStrLn err

