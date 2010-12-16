module GolfScript.Interpreter where

import Control.Monad.State.Lazy
import qualified Data.Map as Map
import Data.Bits (complement)
import Data.List (transpose)
import Data.Char (ord)
import Control.Applicative
import Data.List (intercalate)
import GolfScript.Value
import GolfScript.Parser


coerceTogether :: GolfValue -> GolfValue -> (GolfValue, GolfValue)
coerceTogether a b
  | typePriority a < typePriority b = 
      let a' = coerceTo a b
      in (a', b)
  | typePriority a > typePriority b = 
      let b' = coerceTo b a
      in (a, b')
  | otherwise = (a, b)

coerceTo v (GolfArray _) = GolfArray [v]
coerceTo v (GolfString _) = GolfString $ serialize v
coerceTo (GolfArray vs) (GolfBlock _) = GolfBlock vs
coerceTo (GolfString s) (GolfBlock _) = GolfBlock $ parseString s
coerceTo v to = error $ "Cannot coerce " ++ show v ++ " to " ++ show to

coerced :: Interpreter (GolfValue, GolfValue)
coerced = do b <- vmPop
             a <- vmPop
             return $ coerceTogether a b

ordered :: Interpreter (GolfValue, GolfValue)
ordered = do b <- vmPop
             a <- vmPop
             case typePriority a < typePriority b of
               True ->
                 return (b, a)
               False ->
                 return (a, b)

vmAssign :: String -> GolfValue -> Interpreter ()
vmAssign token v = do vm <- get
                      put $ vm { vmVars = Map.insert token v $ vmVars vm }

vmPop :: Interpreter GolfValue
vmPop = do vm <- get
           case vmStack vm of
             v : stack ->
               do put $ repairBrackets $ vm { vmStack = stack }
                  return v
             [] ->
               error "Popping from empty stack"
    where repairBrackets vm = vm { vmBrackets = map (min $ length $ vmStack vm) $ vmBrackets vm }

vmPush :: GolfValue -> Interpreter ()
vmPush v = do vm <- get
              put $ vm { vmStack = v : vmStack vm }
              
vmDiscard = do vm <- get
               case vmStack vm of
                 v : stack ->
                   put $ vm { vmStack = stack }
                 [] ->
                   return ()

vmColon = do vm <- get
             put $ vm { vmInAssignment = True }

vmPlus = do (a, b) <- coerced
            vmPush $ case coerceTogether a b of
              (GolfNumber a', GolfNumber b') ->
                GolfNumber $ a' + b'
              (GolfArray a', GolfArray b') ->
                GolfArray $ a' ++ b'
              (GolfString a', GolfString b') ->
                GolfString $ a' ++ b'
              (GolfBlock a', GolfBlock b') ->
                GolfBlock $ a' ++ b'
              _ ->
               error $ "Cannot plus " ++ show a ++ " & " ++ show b
              
vmMinus = do (a, b) <- coerced
             vmPush $ case coerceTogether a b of
               (GolfNumber a', GolfNumber b') ->
                 GolfNumber $ a' - b'
               (GolfArray a', GolfArray b') ->
                 GolfArray $ filter (`elem` b') a'
               (GolfString a', GolfString b') ->
                 GolfString $ filter (`elem` b') a'
               _ ->
                 error $ "Cannot minus " ++ show a ++ " & " ++ show b

vmMul = do (a, b) <- ordered
           case (a, b) of
             (GolfNumber a', GolfNumber b') ->
               vmPush $ GolfNumber $ a' * b'
             (GolfArray a', GolfNumber b') ->
               vmPush $ GolfArray $
               take (length a' * b') $ cycle a'
             (GolfString a', GolfNumber b') ->
               vmPush $ GolfString $
               take (length a' * b') $ cycle a'
             (GolfBlock a', GolfNumber b') ->
               mapM_ (const $ run a') [1..b']
             (GolfBlock a', GolfArray b') ->
               do vmPush (head b')
                  mapM_ (\v ->
                             do vmPush v
                                run a'
                        ) (tail b')
             _ ->
               error $ "Cannot mul " ++ show a ++ " & " ++ show b

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
              vmPush $ golfFromBool $ a' == b'
            (GolfString a', GolfNumber b')
              | b' < length a' ->
                vmPush $ GolfNumber $ ord $ a' !! b'
              | otherwise ->
                return ()
            (GolfArray a', GolfNumber b')
              | b' < length a' ->
                vmPush $ a' !! b'
              | otherwise ->
                return ()
            _ ->
                error $ "Cannot eq " ++ show a ++ " & " ++ show b

vmLess = do (a, b) <- ordered
            case (a, b) of
              (GolfNumber a', GolfNumber b') ->
                vmPush $ golfFromBool $ a' < b'
              (GolfArray a', GolfNumber b') 
                | b' >= 0 ->
                    vmPush $ GolfArray $ take b' a'
                | b' < 0 ->
                    vmPush $ GolfArray $ take (length a' + b') a'

vmGreater = do (a, b) <- ordered
               case (a, b) of
                 (GolfNumber a', GolfNumber b') ->
                     vmPush $ golfFromBool $ a' > b'
                 (GolfArray a', GolfNumber b')
                   | b' >= 0 ->
                       vmPush $ GolfArray $ drop b' a'
                   | b' < 0 ->
                       vmPush $ GolfArray $ drop (length a' + b') a'

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

vmBang = do a <- vmPop
            vmPush $ golfFromBool $ isTrue a
                        
vmInspect = do a <- vmPop
               vmPush $ GolfString $ serialize a

vmMod = do (a, b) <- ordered
           case (a, b) of
             (GolfNumber a', GolfNumber b') ->
               vmPush $ GolfNumber $ b' `mod` a'
             (GolfBlock a', GolfArray b') ->
               forM_ b' $ \v -> 
               do vmPush v
                  run a'
             _ ->
               error $ "Cannot % " ++ show a ++ " & " ++ show b
               
vmDec = do a <- vmPop
           case a of
             GolfNumber a' ->
               vmPush $ GolfNumber $ a' - 1
             GolfArray a' ->
               do vmPush $ GolfArray $ tail a'
                  vmPush $ head a'
             GolfString a' ->
               do vmPush $ GolfString $ tail a'
                  vmPush $ GolfNumber $ ord $ head a'
             GolfBlock a' ->
               do vmPush $ GolfBlock $ tail a'
                  vmPush $ head a'

vmInc = do a <- vmPop
           case a of
             GolfNumber a' ->
               vmPush $ GolfNumber $ a' + 1
             GolfArray a' ->
               do vmPush $ GolfArray $ init a'
                  vmPush $ last a'
             GolfString a' ->
               do vmPush $ GolfString $ init a'
                  vmPush $ GolfNumber $ ord $ last a'
             GolfBlock a' ->
               do vmPush $ GolfBlock $ init a'
                  vmPush $ last a'

vmDoWhile = do GolfBlock vs <- vmPop
               let r = do run vs
                          v <- vmPop
                          unless (isFalse v) r
               r

vmRequire = do GolfString fn <- vmPop
               vs <- liftIO $ parseFile fn
               run vs
               
vmZip = do vm <- get
           v <- vmPop
           case v of
             GolfArray a ->
               do let a' = map (\(GolfArray a') -> a') a
                      minLen = minimum $ map length a'
                      a'' = map (take minLen) a'
                      a''' = transpose a''
                      a'''' = map GolfArray a'''
                  vmPush $ GolfArray a''''
             _ ->
               error $ "Cannot zip " ++ show v

vmSin = do GolfNumber a <- vmPop
           vmPush $ GolfNumber $
             truncate $
             (fromIntegral a / 360.0) * 1000

vmPrint = do v <- vmPop
             case v of
               GolfString s ->
                 liftIO $ putStr s
               GolfNumber n ->
                 liftIO $ putStr $ show n
               _ ->
                 error $ "Cannot print " ++ show v

newVM :: VM
newVM = VM { vmStack = [],
             vmBrackets = [],
             vmVars = Map.fromList [(":", GolfBuiltin vmColon),
                                    ("+", GolfBuiltin vmPlus),
                                    ("-", GolfBuiltin vmMinus),
                                    ("*", GolfBuiltin vmMul),
                                    ("/", GolfBuiltin vmDiv),
                                    ("%", GolfBuiltin vmMod),
                                    ("=", GolfBuiltin vmEq),
                                    ("<", GolfBuiltin vmLess),
                                    (">", GolfBuiltin vmGreater),
                                    ("~", GolfBuiltin vmTilde),
                                    ("`", GolfBuiltin vmInspect),
                                    (".", GolfBuiltin vmDup),
                                    ("@", GolfBuiltin vmRotate),
                                    ("\\", GolfBuiltin vmSwap),
                                    (";", GolfBuiltin vmDiscard),
                                    (",", GolfBuiltin vmComma),
                                    ("!", GolfBuiltin vmBang),
                                    ("(", GolfBuiltin vmDec),
                                    (")", GolfBuiltin vmInc),
                                    ("do", GolfBuiltin vmDoWhile),
                                    ("n", GolfString "\n"),
                                    ("h", GolfString "Hello, World"),
                                    ("require", GolfBuiltin vmRequire),
                                    ("zip", GolfBuiltin vmZip),
                                    ("sin", GolfBuiltin vmSin),
                                    ("puts", GolfBuiltin $ vmPrint >> liftIO (putStrLn "")),
                                    ("print", GolfBuiltin vmPrint),
                                    ("p", GolfBuiltin $ vmInspect >> vmPrint >> liftIO (putStrLn ""))],
             vmInAssignment = False
           }
        
run :: [GolfValue] -> Interpreter ()
run = mapM_ (\v ->
                 do liftIO $ putStrLn $ show v
                    run' v
                    vm <- get
                    liftIO $ putStrLn $ "stack: " ++ concatMap serialize (vmStack vm)
            )

run' :: GolfValue -> Interpreter ()
run' (GolfComment _) = return ()
run' (GolfToken token) | token `elem` [" ", "\t", "\r", "\n"] = return ()
run' (GolfToken token) = do vm <- get
                            case vmInAssignment vm of
                              True ->
                                  do put $ vm { vmInAssignment = False }
                                     v <- vmPop
                                     liftIO $ putStrLn $ "assign " ++ show token ++ " := " ++ take 32 (show v)
                                     vmAssign token v
                                     vmPush v
                              False ->
                                  case Map.lookup token $ vmVars vm of
                                    Just v -> 
                                        case v of
                                          GolfBlock vs -> run vs
                                          _ -> run' v
                                    Nothing -> 
                                        error $ "Token undefined: " ++ token ++
                                                   " stack: " ++
                                                   intercalate " " (map serialize $ vmStack vm)
run' (GolfBuiltin b) = b
run' (GolfArray vs) = do vm <- get
                         let stack = vmStack vm
                         put $ vm { vmBrackets = length stack : vmBrackets vm }
                         
                         run vs
                         
                         vm' <- get
                         let stack' = vmStack vm'
                             bracket : brackets = vmBrackets vm'
                             (as, stack'') = splitAt (length stack' - bracket) $ vmStack vm'
                         put $ vm' { vmBrackets = brackets, 
                                     vmStack = GolfArray (reverse as) : stack'' }
run' v = vmPush v

exec :: VM -> [GolfValue] -> IO VM
exec vm code = execStateT (run code) vm
