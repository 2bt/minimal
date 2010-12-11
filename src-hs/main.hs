module Main where

import System.Environment
import Control.Monad
import qualified GolfScript

runFile :: String -> IO ()
runFile fn = do code <- GolfScript.parseFile fn
                putStrLn $ show code

main = getArgs >>=
       mapM runFile
