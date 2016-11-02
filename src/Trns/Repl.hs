module Trns.Repl where

import System.Exit
import Trns.Parser
import Control.Monad
import Lang.AST
import Trns.Interpreter
import Lang.Interpreter

main :: IO ()
main = repl makeProgram makeEnv

repl :: Prog -> Env -> IO ()
repl prog env = do
  putStr "-> "
  line <- getLine

  when (line == ".e") exitSuccess

  when (take 2 line == ".l") $ do
    progAndEnv <- runFile ((strip $ drop 3 line) ++ ".trns") prog env
    printProgAndLoop progAndEnv

  when (take 2 line == ".r") $ do
    printProg $ (interpret prog, env)
    repl prog env

  -- runLine
  printProgAndLoop $ runLine line prog env

printProg :: (Prog, Env) -> IO ()
printProg progAndEnv = let s = show $ fst progAndEnv
                       in do putStr $ (take ((length s) - 2) . drop 1) s
                             putStr "\n\n"

printProgAndLoop :: (Prog, Env) -> IO ()
printProgAndLoop progAndEnv = do
  printProg progAndEnv
  repl (fst progAndEnv) (snd progAndEnv)

runLine :: String -> Prog -> Env -> (Prog, Env)
runLine code prog env = runCmd prog env cmd
  where cmd = extractValue $ readTrnsCmd code

runFile :: FilePath -> Prog -> Env -> IO (Prog, Env)
runFile file prog env = do
  contents <- readFile file
  let script = extractValue $ readTrnsScript contents
  return $ runScript prog env script

strip = lstrip . rstrip
lstrip = dropWhile (`elem` " \t")
rstrip = reverse . lstrip . reverse

extractValue :: Either e a -> a
extractValue (Left _) = error "Something went wrong"
extractValue (Right val) = val
