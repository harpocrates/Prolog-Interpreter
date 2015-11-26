import Prolog.Base
import Prolog.Interpreter

import Control.Monad      (forM)
import Data.Either        (partitionEithers)
import Data.List          (intercalate)
import System.Environment (getArgs)
import System.IO          (openFile, IOMode(..), hSetBuffering, BufferMode(..), stdin, stdout)

main :: IO ()
main = do
  -- Load the standard prelude
  (Right prelude) <- openFile "prelude.pl" ReadMode >>= loadProgram "prelude.pl"

  -- Load the other programs
  sourcePaths <- getArgs
  program <- forM sourcePaths $ \s -> openFile s ReadMode >>= loadProgram s
  let (errors,successes) = partitionEithers program

  -- set `stdout` to buffer output after every character
  hSetBuffering stdout $ BlockBuffering $ Just 1

  if null errors
    then do
      putStrLn "Compiled successfully."
      putStrLn $ "File(s) loaded: " ++ intercalate ", " sourcePaths ++ "."
      repl stdin stdout "> " $ prelude ++ concat successes
    else do
      putStrLn "Failed to compile."
      putStrLn $ "\n" ++ intercalate "\n\n" errors
