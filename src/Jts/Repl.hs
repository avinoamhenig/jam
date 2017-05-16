module Jts.Repl where

import System.Exit
import Jts.Parser
import Control.Monad
import Jam.Ast hiding (Env)
import Jts.Ast
import Jts.Interpreter
import Jam.Interpreter
import Jam.Accessors
import Jam.Suggestions
import Jam.Basis
import Data.Map hiding (map)
import Prelude hiding (lookup)
import System.Console.Haskeline
import Control.Monad.Trans.Class (lift)
import Control.Monad.Except (runExcept)
import qualified Data.Set as Set

main :: IO ()
main = runInputT defaultSettings $ repl makeProgram makeEnv

repl :: Prog -> Env -> InputT IO ()
repl prog env = do
  maybeLine <- getInputLine "-> "
  case maybeLine of
    Nothing -> lift exitSuccess
    Just _line -> let line = strip _line in
      if line == "" || (((length line) > 1) && ((take 2) line == "--"))
        then repl prog env
        else do
          do (prog', env') <- execLine prog env line
             when (take 1 line /= ".") $ printProgAndLoop (prog', env')
             repl prog' env'

execLine :: Prog -> Env -> String -> InputT IO (Prog, Env)
execLine prog env line = let line' = strip line in
  if line' == "" || (((length line') > 1) && ((take 2) line' == "--"))
    then return (prog, env)
    else if take 1 line' == "."
          then doCommand prog env $ words $ drop 1 $ strip line'
          else runLine line' prog env

doCommand :: Prog -> Env -> [String] -> InputT IO (Prog, Env)
doCommand prog env (cmd:args)
  | cmd == "q" = lift exitSuccess
  | cmd == "l" = runFile ((args !! 0) ++ ".jts") prog env
  | cmd == "e" = do let progOrE = interpret prog
                    case runExcept progOrE of
                      Left e -> outputStr $ (show e) ++ "\n\n"
                      Right prog -> printProg $ (prog, env)
                    return (prog, env)
  | cmd == "t" || cmd == "it" =
    let (prefix:name) = args !! 0
        initial = cmd == "it"
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
        Just t -> outputStr $ (deparen $ show (if initial then t
                                                else finalType prog t))
                              ++ "\n\n"
      return (prog, env)
  | cmd == "c" =
    let (prefix:_name) = args !! 0
        name = if prefix == '@' then _name else prefix:_name
        u' = (reads name)::[(Int, String)]
    in do
      when ((length u') == 0) $ do outputStr "Invalid type-variable name\n\n"
                                   repl prog env
      let (u, _) = u' !! 0
      case lookup (TyVar u []) (tyVarMap prog) of
        Nothing -> outputStr ""
        Just t -> outputStr ("@" ++ name ++ " |-> " ++ (show t) ++ "\n")
      case lookup (TyVar u []) (univTyVarMap prog) of
        Nothing -> outputStr ""
        Just constrs -> outputStr $ unlines (map (_showConstr prog)
                                                 (Set.toList constrs))
      outputStr "\n"
      return (prog, env)
  | cmd == "s" =
    let (prefix:_name) = args !! 0
        name = if prefix == ':' then _name else prefix:_name
        ename = ExpName name
    in case lookup ename (expNames env) of
        Nothing -> do outputStr "\n"
                      return (prog, env)
        Just path -> do outputStr . unlines $
                          map unlines (map (map show) (suggestions prog path ename))
                        return (prog, env)
  | cmd == "p" = do printProg (prog, env)
                    return (prog, env)
  where _showConstr p (UnivTyVarConstraint utv ntv _) =
          (show (finalType p (TyVarType utv))) ++ " => "
            ++ (show (finalType p (TyVarType ntv)))
doCommand prog env _ = do outputStr "Unrecognized command.\n"
                          return (prog, env)

runFile :: FilePath -> Prog -> Env -> InputT IO (Prog, Env)
runFile file prog env = _runFile `catch` handleError
  where _runFile = do
          contents <- lift $ readFile file
          let cmds = lines contents
          progAndEnv <- execLines prog env cmds
          return progAndEnv
        handleError :: IOException -> InputT IO (Prog, Env)
        handleError _ = do outputStr "File not found.\n\n"
                           return (prog, env)
        execLines :: Prog -> Env -> [String] -> InputT IO (Prog, Env)
        execLines prog env [] = return (prog, env)
        execLines prog env (line:cmds) = do
          (prog', env') <- execLine prog env line
          progAndEnv <- execLines prog' env' cmds
          return progAndEnv

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

strip = lstrip . rstrip
lstrip = dropWhile (`elem` " \t\n")
rstrip = reverse . lstrip . reverse
