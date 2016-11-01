module Trns.Repl where

import System.Exit
import Trns.Parser
import Control.Monad
import Control.Monad.Except
import Lang.AST
import Trns.Interpreter

main :: IO ()
main = repl makeProgram makeEnv

repl :: Prog -> Env -> IO ()
repl prog env = do
  putStr "-> "
  line <- getLine
  when (line == ".e") exitSuccess
  when (take 2 line == ".l") $ do
    progAndEnv <- runFile ((strip $ drop 3 line) ++ ".trns") prog env
    putStr $ show $ fst progAndEnv
    putStr "\n\n"
    repl (fst progAndEnv) (snd progAndEnv)

  -- runLine
  let progAndEnv = runLine line prog env
      newProg = fst progAndEnv
      newEnv = snd progAndEnv
  putStr $ show newProg
  putStr "\n\n"
  repl newProg newEnv

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

trapError :: (Show a, MonadError a m) => m String -> m String
trapError action = action `catchError` (return . show)
