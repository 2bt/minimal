module Main where

import System.Environment
import Control.Monad
import GolfScript.Parser (parseFile)
import GolfScript.Value (serialize)
import GolfScript.Interpreter (runCode)

runFile :: String -> IO ()
runFile fn = do code <- parseFile fn
                stack <- runCode code
                putStrLn $ concatMap serialize stack

main = getArgs >>=
       mapM runFile
