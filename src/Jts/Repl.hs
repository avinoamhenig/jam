module Jts.Repl where

import System.Exit
import Jts.Parser
import Control.Monad
import Jam.Ast hiding (Env)
import Jts.Ast
import Jts.Interpreter
import Jam.Interpreter
import Jam.Accessors
import Jam.Basis
import Data.Map
import Prelude hiding (lookup)
import System.Console.Haskeline
import Control.Monad.Trans.Class (lift)
import Control.Monad.Except (runExcept)

main :: IO ()
main = runInputT defaultSettings $ repl makeProgram makeEnv

repl :: Prog -> Env -> InputT IO ()
repl prog env = do
  maybeLine <- getInputLine "-> "
  case maybeLine of
    Nothing -> lift exitSuccess
    Just _line ->
      let line = strip _line
      in do
        when (take 1 line == ".") $ doCommand prog env $ words $ drop 1 line
        progAndEnv <- runLine line prog env
        printProgAndLoop progAndEnv

doCommand :: Prog -> Env -> [String] -> InputT IO ()
doCommand prog env (cmd:args)
  | cmd == "q" || cmd == "quit" = lift exitSuccess
  | cmd == "l" || cmd == "load" = do
      progAndEnv <- runFile ((args !! 0) ++ ".jts") prog env
      printProgAndLoop progAndEnv
  | cmd == "e" || cmd == "eval" = do let progOrE = interpret prog
                                     case runExcept progOrE of
                                       Left e -> outputStr $ (show e) ++ "\n\n"
                                       Right prog -> printProg $ (prog, env)
                                     repl prog env
  | cmd == "t" || cmd == "typeof" =
    let (prefix:name) = args !! 0
        t = if prefix /= ':'
            then let iname = if prefix == '$' then name else prefix:name
                 in case lookup (IdName iname) (idNames env) of
                      Nothing -> case lookup iname (basisIds basis) of
                        Nothing -> Nothing
                        Just (Id _ _ t) -> Just t
                      Just (Id _ _ t) -> Just t
            else let ename = ExpName name
                 in case lookup ename (expNames env) of
                      Nothing -> Nothing
                      Just path -> case expAtPath prog path of
                        Nothing -> Nothing
                        Just e -> Just $ getType e
    in do
      case t of
        Nothing -> outputStr "Expression or identifier does not exist\n\n"
        Just t -> outputStr $ (deparen $ show (finalType prog t)) ++ "\n\n"
      repl prog env
doCommand prog env _ = do outputStr "Unrecognized command.\n"
                          repl prog env

printProg :: (Prog, Env) -> InputT IO ()
printProg progAndEnv = let s = show $ fst progAndEnv
                       in do outputStr $ s ++ "\n\n"

printProgAndLoop :: (Prog, Env) -> InputT IO ()
printProgAndLoop progAndEnv = do
  printProg progAndEnv
  repl (fst progAndEnv) (snd progAndEnv)

runLine :: String -> Prog -> Env -> InputT IO (Prog, Env)
runLine code prog env = case readJtsCmd code of
                          Left e -> do outputStr $ (show e) ++ "\n\n"
                                       return (prog, env)
                          -- Uncomment below to just print parsed cmd
                          -- Right cmd -> do outputStr $ (show cmd) ++ "\n\n"
                          --                 return $ (prog, env)
                          Right cmd -> case runExcept $ runCmd prog env cmd of
                            Left e -> do outputStr $ (show e) ++ "\n\n"
                                         return (prog, env)
                            Right (prog, env) -> return (prog, env)

runFile :: FilePath -> Prog -> Env -> InputT IO (Prog, Env)
runFile file prog env = _runFile `catch` handleError
  where _runFile = do
          contents <- lift $ readFile file
          case readJtsScript contents of
            Left e -> do outputStr $ (show e) ++ "\n\n"
                         return (prog, env)
            Right script -> case runExcept $ runScript prog env script of
              Left e -> do outputStr $ (show e) ++ "\n\n"
                           return (prog, env)
              Right (prog, env) -> return (prog, env)
        handleError :: IOException -> InputT IO (Prog, Env)
        handleError _ = do outputStr "File not found.\n\n"
                           return (prog, env)

strip = lstrip . rstrip
lstrip = dropWhile (`elem` " \t")
rstrip = reverse . lstrip . reverse