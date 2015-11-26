module Prolog.Interpreter
  ( loadProgram, repl
  ) where

import qualified Prolog.Parsing as P
import Prolog.Printing
import Prolog.Base

import Text.Parsec
import System.IO
import Control.Monad

-- Loads a program from a given handle and
-- attempts to parse it.
loadProgram :: String -> Handle -> IO (Either String Program)
loadProgram name hdl = do
  source <- hGetContents hdl
  return $ case parse P.program name source of
    Right a  -> Right a
    Left err -> Left $ show err

-- Starts a repl with given input and output handles,
-- given prompt and off of a given program
repl :: Handle -> Handle -> String -> Program -> IO ()
repl hIn hOut prompt program = do
    hPutStr hOut prompt
    input <- hGetLine hIn

    if (input == "halt.")
      then return ()
      else do
        case parse P.query "" input of
          Left err -> hPutStrLn hOut $ "Invalid query: " ++ show err
          Right qs -> showOutput hIn hOut $ query program qs

        repl hIn hOut prompt program
  where
    showOutput :: Handle -> Handle -> [Substitution] -> IO ()
    showOutput hIn hOut []     = hPutStrLn hOut "no."
    showOutput hIn hOut (θ:θs) = do
      if null θ
        then hPutStr hOut "yes"
        else hPutStr hOut $ showSubstitution θ
      c <- hGetChar hIn
      hGetChar hIn
      case c of '.' -> return ()
                ';' -> showOutput hIn hOut θs