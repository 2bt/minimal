module Main where

import System.Environment
import Control.Monad
import GolfScript.Parser (parseFile)
import GolfScript.Value (serialize)
import GolfScript.Interpreter (runCode)
import qualified Synth as S
import SynthOps (boot)

runFile :: S.SynthRef -> String -> IO ()
runFile s fn = do code <- parseFile fn
                  stack <- runCode $ boot s : code
                  putStrLn $ concatMap serialize stack

main = do s <- S.new
          getArgs >>= mapM (runFile s)
          S.close s
