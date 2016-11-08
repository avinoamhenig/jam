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
  line <- (liftM strip) getLine
  when (take 1 line == ".") $ doCommand prog env $ words $ drop 1 line
  progAndEnv <- runLine line prog env
  printProgAndLoop progAndEnv

doCommand :: Prog -> Env -> [String] -> IO ()
doCommand prog env (cmd:args)
  | cmd == "q" || cmd == "quit" = exitSuccess
  | cmd == "l" || cmd == "load" = do
      progAndEnv <- runFile ((args !! 0) ++ ".trns") prog env
      printProgAndLoop progAndEnv
  | cmd == "e" || cmd == "eval" = do printProg $ (interpret prog, env)
                                     repl prog env
doCommand prog env _ = do putStr "Unrecognized command.\n"
                          repl prog env

printProg :: (Prog, Env) -> IO ()
printProg progAndEnv = let s = show $ fst progAndEnv
                       in do putStr $ s ++ "\n\n"

printProgAndLoop :: (Prog, Env) -> IO ()
printProgAndLoop progAndEnv = do
  printProg progAndEnv
  repl (fst progAndEnv) (snd progAndEnv)

runLine :: String -> Prog -> Env -> IO (Prog, Env)
runLine code prog env = case readTrnsCmd code of
                          Left e -> do putStr $ (show e) ++ "\n\n"
                                       return $ (prog, env)
                          Right cmd -> return $ runCmd prog env cmd

runFile :: FilePath -> Prog -> Env -> IO (Prog, Env)
runFile file prog env = do
  contents <- readFile file
  case readTrnsScript contents of
    Left e -> do putStr $ (show e) ++ "\n\n"
                 return $ (prog, env)
    Right script -> return $ runScript prog env script

strip = lstrip . rstrip
lstrip = dropWhile (`elem` " \t")
rstrip = reverse . lstrip . reverse
