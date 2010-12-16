{-# LANGUAGE TypeSynonymInstances #-}
module GolfScript.Value where

import Data.Map (Map)
import Control.Monad.State.Lazy (StateT)
import Data.List (intercalate)

data GolfValue = GolfComment String
               | GolfNumber Int
               | GolfArray [GolfValue]
               | GolfString String
               | GolfToken String
               | GolfBlock [GolfValue]
               | GolfBuiltin (Interpreter ())
               deriving (Show, Eq)

serialize :: GolfValue -> String
serialize (GolfComment c) = "#" ++ c ++ "\n"
serialize (GolfNumber n) = show n
serialize (GolfArray vs) = "[" ++ intercalate " " (map serialize vs) ++ "]"
serialize (GolfString s) = "'" ++ s ++ "'"
serialize (GolfToken token) = token
serialize (GolfBlock vs) = "{" ++ concatMap serialize vs ++ "}"
  
isFalse :: GolfValue -> Bool
isFalse = (`elem` [GolfNumber 0, GolfArray [], GolfString "", GolfBlock []])
isTrue = not . isFalse

golfFromBool :: Bool -> GolfValue
golfFromBool b = GolfNumber $
                 if b
                 then 1
                 else 0

typePriority (GolfComment _) = 0
typePriority (GolfNumber _) = 1
typePriority (GolfArray _) = 2
typePriority (GolfString _) = 3
typePriority (GolfToken _) = 4
typePriority (GolfBlock _) = 5
typePriority (GolfBuiltin _) = 4


data VM = VM { vmStack :: [GolfValue],
               vmBrackets :: [Int],
               vmVars :: Map String GolfValue,
               vmInAssignment :: Bool
             }
        deriving (Show)
type Interpreter a = StateT VM IO a

  
instance Eq (Interpreter a) where
  _ == _ = True
instance Ord (Interpreter a) where
  compare _ _ = EQ
instance Show (Interpreter a) where
  show _ = "<code>"

