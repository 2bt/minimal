{-# LANGUAGE TypeSynonymInstances #-}
module GolfScript.Value where

import Data.Map (Map)
import Control.Monad.State.Lazy (StateT)

data GolfValue = GolfAssign String
               | GolfComment String
               | GolfNumber Int
               | GolfArray [GolfValue]
               | GolfString String
               | GolfToken String
               | GolfBlock [GolfValue]
               | GolfBuiltin (Interpreter ())
               deriving (Show, Eq, Ord)

serialize :: GolfValue -> String
serialize (GolfAssign id) = ":" ++ id
serialize (GolfComment c) = c ++ "\n"
serialize (GolfNumber n) = show n
serialize (GolfArray vs) = "[" ++ concatMap serialize vs ++ "]"
serialize (GolfString s) = "'" ++ s ++ "'"
serialize (GolfToken token) = token
serialize (GolfBlock vs) = "{" ++ concatMap serialize vs ++ "}"
  
isFalse :: GolfValue -> Bool
isFalse = (`elem` [GolfNumber 0, GolfArray [], GolfString "", GolfBlock []])


data VM = VM { vmStack :: [GolfValue],
               vmVars :: Map String GolfValue
             }
type Interpreter a = StateT VM IO a

  
instance Eq (Interpreter a) where
  _ == _ = True
instance Ord (Interpreter a) where
  compare _ _ = EQ
instance Show (Interpreter a) where
  show _ = "<code>"

