module GolfScript.Interpreter where

import Control.Monad.State.Lazy
import qualified Data.Map as Map
import Data.Bits (complement)
import Data.List (transpose)
import Control.Applicative
import GolfScript.Value
import GolfScript.Parser


coerceTogether :: GolfValue -> GolfValue -> (GolfValue, GolfValue)
coerceTogether a b
  | a < b = let a' = coerceTo a b
            in (a', b)
  | a > b = let b' = coerceTo b a
            in (a, b')
  | otherwise = (a, b)

coerceTo v (GolfArray _) = GolfArray [v]
coerceTo v (GolfString _) = GolfString $ serialize v
coerceTo v to = error $ "Cannot coerce " ++ show (serialize v) ++ " to " ++ show (serialize to)

coerced = do b <- vmPop
             a <- vmPop
             return $ coerceTogether a b

ordered :: Interpreter (GolfValue, GolfValue)
ordered = do b <- vmPop
             a <- vmPop
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

vmMul = do (a, b) <- ordered
           case (a, b) of
             (GolfArray a', GolfNumber b') ->
               vmPush $ GolfArray $
               take (length a' * b') $ cycle a'

vmDiv = do (a, b) <- ordered
           case (a, b) of
             (GolfNumber a', GolfNumber b') ->
               vmPush $ GolfNumber $ a' `div` b'
             (GolfArray a', GolfNumber b') ->
               let r vs = if length vs > b'
                          then let (vs', vs'') = splitAt b' vs
                               in (GolfArray vs') : (r vs'')
                          else [GolfArray vs]
               in vmPush $ GolfArray $ r a'
             (GolfString a', GolfNumber b') ->
               let r vs = if length vs > b'
                          then let (vs', vs'') = splitAt b' vs
                               in (GolfString vs') : (r vs'')
                          else [GolfString vs]
               in vmPush $ GolfArray $ r a'
             (GolfBlock a', GolfArray b') ->
               mapM_ (\v ->
                       vmPush v >>
                       run a'
                     ) b'
             _ ->
               error $ "Cannot div " ++ show a ++ " & " ++ show b

vmEq = do (a, b) <- ordered
          case (a, b) of
            (GolfNumber a', GolfNumber b') ->
              vmPush $ GolfNumber $
              if a' == b'
              then 1
              else 0
            (GolfArray a', GolfNumber b')
              | b' < length a' ->
                vmPush $ a' !! b'
              | otherwise ->
                return ()

vmTilde = do v <- vmPop
             case v of
               GolfNumber n ->
                 vmPush $ GolfNumber $ complement n
               GolfArray vs ->
                 mapM_ vmPush vs
               GolfString s ->
                 run $ parseString s
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

vmComma = do a <- vmPop
             case a of
               GolfNumber a' ->
                 vmPush $ GolfArray $ map GolfNumber [0..(a' - 1)]
               GolfArray a' ->
                 vmPush $ GolfNumber $ length a'
               GolfBlock a' ->
                 do b <- vmPop
                    case b of
                      GolfArray b' ->
                        do b'' <- filterM (\v ->
                                            do vmPush v
                                               run a'
                                               isFalse <$> vmPop
                                          ) b'
                           vmPush $ GolfArray b''

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
                            unless (isFalse v) run
               run

vmRequire = do GolfString fn <- vmPop
               vs <- liftIO $ parseFile fn
               run vs
               
vmZip = do vm <- get
           liftIO $ putStrLn $ "stack: " ++ concatMap serialize (vmStack vm)
           v <- vmPop
           case v of
             GolfArray a ->
               do let a' = map (\(GolfArray a') -> a') a
                      a'' = transpose a'
                      a''' = map GolfArray a''
                  vmPush $ GolfArray a'''
             _ ->
               error $ "Cannot zip " ++ show v

newVM :: VM
newVM = VM { vmStack = [],
             vmVars = Map.fromList [(" ", stub),
                                    ("\t", stub),
                                    ("\r", stub),
                                    ("\n", stub),
                                    ("+", GolfBuiltin vmPlus),
                                    ("-", GolfBuiltin vmMinus),
                                    ("*", GolfBuiltin vmMul),
                                    ("/", GolfBuiltin vmDiv),
                                    ("=", GolfBuiltin vmEq),
                                    ("~", GolfBuiltin vmTilde),
                                    (".", GolfBuiltin vmDup),
                                    ("@", GolfBuiltin vmRotate),
                                    ("\\", GolfBuiltin vmSwap),
                                    (";", GolfBuiltin vmDiscard),
                                    (",", GolfBuiltin vmComma),
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
                              Nothing -> error $ "Token undefined: " ++ token ++
                                         " stack: " ++ concatMap serialize (vmStack vm)
run' (GolfBuiltin b) = b
run' (GolfAssign token) = do v <- vmPop
                             vm <- get
                             put $ vm { vmVars = Map.insert token v $ vmVars vm }
run' (GolfArray vs) = do vm <- get
                         let stack = vmStack vm
                         put $ vm { vmStack = [] }
                         run vs
                         vm' <- get
                         let stack' = vmStack vm'
                         put $ vm' { vmStack = stack }
                         vmPush $ GolfArray stack'
run' v = vmPush v

runCode code = vmStack <$>
               execStateT (run code) newVM
