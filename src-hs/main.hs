module Main where

import System.Environment
import Control.Monad
import GolfScript.Parser (parseFile)
import GolfScript.Value (GolfValue(GolfToken), serialize)
import GolfScript.Interpreter (exec, newVM)
import qualified Synth as S
import SynthOps (boot)

runFile :: S.SynthRef -> String -> IO ()
runFile s fn = do code <- parseFile fn
                  let lööp vm = do vm' <- exec vm [GolfToken "tick"]
                                   S.play s
                                   --lööp vm'
                  exec newVM (boot s : code) >>= lööp
                  

main = do s <- S.new
          getArgs >>= mapM (runFile s)
          S.close s
