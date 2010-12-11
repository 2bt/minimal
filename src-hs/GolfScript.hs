{-# LANGUAGE TypeSynonymInstances #-}
module GolfScript where

import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Char
import Control.Applicative hiding ((<|>), many)
import Control.Monad.State.Lazy
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bits (complement)
import Data.List (transpose)

-- TODO: order
data GolfValue = GolfAssign String
               | GolfComment String
               | GolfNumber Int
               | GolfArray [GolfValue]
               | GolfString String
               | GolfToken String
               | GolfBlock [GolfValue]
               | GolfBuiltin (Interpreter ())
               deriving (Eq, Ord)
                        
data VM = VM { vmStack :: [GolfValue],
               vmVars :: Map String GolfValue
             }
type Interpreter a = StateT VM IO a

instance Show GolfValue where
  show (GolfAssign id) = ":" ++ id
  show (GolfComment c) = c ++ "\n"
  show (GolfNumber n) = show n
  show (GolfArray vs) = "[" ++ concatMap show vs ++ "]"
  show (GolfString s) = "'" ++ s ++ "'"
  show (GolfToken token) = token
  show (GolfBlock vs) = "{" ++ concatMap show vs ++ "}"
  
instance Eq (Interpreter a) where
  _ == _ = True
instance Ord (Interpreter a) where
  compare _ _ = EQ

parseFile :: FilePath -> IO [GolfValue]
parseFile f = right <$> (parseFromFile $ many golfCode) f
  where right (Left e) = error $ show e
        right (Right a) = a

golfCode = golfAssign <|>
           golfNumber <|>
           golfArray <|>
           golfBlock <|>
           golfString <|>
           golfRawString <|>
           golfComment <|>
           golfToken
golfAssign = do char ':'
                GolfToken token <- golfToken
                return $ GolfAssign token
golfNumber = GolfNumber <$> read <$> many1 digit
golfBlock = char '{' >>
            (GolfBlock <$> manyTill golfCode (char '}'))
golfArray = char '[' >>
            (GolfBlock <$> manyTill golfCode (char ']'))
-- TODO: escapes
golfString = char '\'' >>
             (GolfString <$> manyTill anyChar (char '\''))
-- TODO: escapes
golfRawString = char '"' >>
                (GolfString <$> manyTill anyChar (char '"'))
golfComment = char '#' >>
              (GolfComment <$> manyTill anyChar (char '\n'))
golfToken = GolfToken <$> (word <|> ((:"") <$> noneOf "}]"))
  where word = do h <- letter <|> char '_'
                  t <- many $ alphaNum <|> char '_'
                  return $ h : t

coerceTogether :: GolfValue -> GolfValue -> (GolfValue, GolfValue)
coerceTogether a b
  | a < b = let a' = coerceTo a b
            in (a', b)
  | a > b = let b' = coerceTo b a
            in (a, b')
  | otherwise = (a, b)

coerceTo v (GolfArray _) = GolfArray [v]
coerceTo v (GolfString _) = GolfString $ show v
coerceTo v to = error $ "Cannot coerce " ++ show v ++ " to " ++ show to

coerced = do b <- vmPop
             a <- vmPop
             return $ coerceTogether a b

ordered :: Interpreter (GolfValue, GolfValue)
ordered = do a <- vmPop
             b <- vmPop
             case a < b of
               True ->
                 return (b, a)
               False ->
                 return (a, b)


vmPop :: Interpreter GolfValue
vmPop = do vm <- get
           case vmStack vm of
             v : stack ->
               do put $ vm { vmStack = stack }
                  return v
             [] ->
               error "Popping from empty stack"

vmPush :: GolfValue -> Interpreter ()
vmPush v = do vm <- get
              put $ vm { vmStack = v : vmStack vm }

vmDiscard = do vm <- get
               case vmStack vm of
                 v : stack ->
                   put $ vm { vmStack = stack }
                 [] ->
                   return ()

vmPlus = do (a, b) <- coerced
            vmPush $ case coerceTogether a b of
              (GolfNumber a', GolfNumber b') ->
                GolfNumber $ a' + b'
              (GolfArray a', GolfArray b') ->
                GolfArray $ a' ++ b'
              (GolfString a', GolfString b') ->
                GolfString $ a' ++ b'
              
vmMinus = do (a, b) <- coerced
             vmPush $ case coerceTogether a b of
               (GolfNumber a', GolfNumber b') ->
                 GolfNumber $ a' - b'
               (GolfArray a', GolfArray b') ->
                 GolfArray $ filter (`elem` b') a'
               (GolfString a', GolfString b') ->
                 GolfString $ filter (`elem` b') a'

vmTilde = do v <- vmPop
             case v of
               GolfNumber n ->
                 vmPush $ GolfNumber $ complement n
               GolfArray vs ->
                 mapM_ vmPush vs
               GolfString s ->
                 let Right vs = parse (many golfCode) "-" s
                 in run vs
               GolfBlock vs ->
                 run vs

vmDup = do v <- vmPop
           vmPush v
           vmPush v

vmRotate = do a <- vmPop
              b <- vmPop
              c <- vmPop
              vmPush b
              vmPush a
              vmPush c

vmSwap = do a <- vmPop
            b <- vmPop
            vmPush a
            vmPush b

--vmDiv = GolfBuiltin $

{-vmMod = GolfBuiltin $
        do a <- vmPop
           b <- vmPop
           case a < b of
             True ->
               do vmPush a
                  vmPush b
                  vmMod
             False ->
               case (a, b) of
                 (GolfNumber a', GolfNumber b') ->
                   vmPush $ GolfNumber $ b' `mod` a'-}

vmDoWhile = do GolfBlock vs <- vmPop
               let run = do mapM_ run' vs
                            v <- vmPop
                            case v of
                              GolfNumber 0 -> return ()
                              _ -> run
               run

vmRequire = do GolfString fn <- vmPop
               vs <- liftIO $ parseFile fn
               run vs
               
vmZip = do vm <- get
           liftIO $ putStrLn $ "stack: " ++ show (vmStack vm)
           GolfArray a <- vmPop
           let a' = map (\(GolfArray a') -> a') a
               a'' = transpose a'
               a''' = map GolfArray a''
           vmPush $ GolfArray a'''

newVM :: VM
newVM = VM { vmStack = [],
             vmVars = Map.fromList [(" ", stub),
                                    ("\t", stub),
                                    ("\r", stub),
                                    ("\n", stub),
                                    ("+", GolfBuiltin vmPlus),
                                    ("-", GolfBuiltin vmMinus),
                                    ("~", GolfBuiltin vmTilde),
                                    (".", GolfBuiltin vmDup),
                                    ("@", GolfBuiltin vmRotate),
                                    ("\\", GolfBuiltin vmSwap),
                                    (";", GolfBuiltin $ vmDiscard),
                                    ("do", GolfBuiltin vmDoWhile),
                                    ("n", GolfString "\n"),
                                    ("h", GolfString "Hello, World"),
                                    ("require", GolfBuiltin vmRequire),
                                    ("zip", GolfBuiltin vmZip)]
           }
  where stub = GolfBuiltin $ return ()
        
run :: [GolfValue] -> Interpreter ()
run = mapM_ run'

run' (GolfComment _) = return ()
run' (GolfToken token) = do vm <- get
                            liftIO $ putStrLn $ "run " ++ show token
                            case Map.lookup token $ vmVars vm of
                              Just v -> run' v
                              Nothing -> error $ "Token undefined: " ++ token
run' (GolfBuiltin b) = b
run' (GolfAssign token) = do v <- vmPop
                             vm <- get
                             put $ vm { vmVars = Map.insert token v $ vmVars vm }
run' v = vmPush v

runCode code = vmStack <$>
               execStateT (run code) newVM
