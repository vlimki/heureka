module Main (main) where

import Lib
import System.Environment
import System.Exit
import System.IO
import System.Directory
import System.Process
import Control.Monad (when, void, unless)

usage :: String
usage = version ++ "\nUsage: heureka [help|setup]\nhelp - Prints this help menu\nsetup - Sets up a git repository"

exec :: String -> [String] -> IO ExitCode
exec cmd args = do
  (_, _, _, h) <- createProcess (proc cmd args)
  waitForProcess h

version :: String
version = "heureka 0.0.1 - back up ideas to git"

dirPath :: String -> String
dirPath user = if user == "root" then "/root/.heureka" else "/home/" ++ user ++ "/.heureka"

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse [] = new
parse ["help"] = putStrLn usage >> exitSuccess
parse ["setup"] = setup
parse _ = putStrLn usage >> exitSuccess

new :: IO ()
new = do
  user <- getEnv "USER"
  exists <- doesDirectoryExist $ dirPath user
  isRepo <- isGitRepo

  unless (exists && isRepo) $ do
    putStrLn "Your data directory has not been initialized. Run `heureka setup` first."

setup :: IO ()
setup = do
  user <- getEnv "USER"
  exists <- doesDirectoryExist $ dirPath user

  if exists then do
    putStrLn $ dirPath user ++ " already exists. Using it instead of creating a new directory."
  else do
    exitCode <- exec "mkdir" [dirPath user]
    when (exitCode /= ExitSuccess) $ do
      putStrLn "Error when creating new directory. Exiting..." >> exitWith exitCode

  setCurrentDirectory $ dirPath user
  r <- isGitRepo
  when r $ do
    putStrLn "Directory is already a git repository. Exiting..." >> exitSuccess

  putStr "Git origin: "
  hFlush stdout
  origin <- getLine

  void $ exec "git" ["init"]
  exitCode <- exec "git" ["remote", "add", "origin", origin]
  when (exitCode /= ExitSuccess) $ do
    putStrLn "Git directory"

isGitRepo :: IO Bool
isGitRepo = do
  result <- readProcess "git" ["rev-parse", "--is-inside-work-tree"] ""
  return $ result == "true\n"

