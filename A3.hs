{-|
 -
Module:      A3
Description: Assignment 3
Copyright: (c) University of Toronto Mississauga, 2022
               CSC324 Principles of Programming Languages, Fall 2022
-}
-- This lists what this module exports. Don't change this!

module A3 (
    -- Warmup Task
    cpsFactorial, cpsFibonacci, cpsLength, cpsMap,
    cpsMergeSort, cpsSplit, cpsMerge,
    -- Main Task
    cpsEval
) where

-- You *may not* add imports from Data.Map, or any other imports
import qualified Data.Map (Map, lookup, insert, empty, fromList)
import A3Types (Env, emptyEnv, Value(..), HaskellProc(..), Expr(..))


------------------------------------------------------------------------------
-- * Warmup Task. CPS Transforming Haskell Functions *
------------------------------------------------------------------------------

-- | Compute the factorial of a number
-- factorial :: Int -> Int

-- | Compute the factorial of a number, in continuation passing style
cpsFactorial:: Int -> (Int -> r) -> r
cpsFactorial 0 k = k 1
cpsFactorial n k = cpsFactorial (n-1) (\res -> k (n * res))

-- | Compute the n-th fibonacci number F(n).
--    Recall F(0) = 0, F(1) = 1, and F(n) = F(n-1) + F(n-2)

-- fibonacci :: Int -> Int
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci (n - 1)) + (fibonacci (n - 2))

-- | Compute the n-th fibonacci number F(n), in continuation passing style
cpsFibonacci:: Int -> (Int -> r) -> r
cpsFibonacci 0 k = k 0 
cpsFibonacci 1 k = k 1
cpsFibonacci n k = cpsFibonacci (n-1) (\res1 -> cpsFibonacci (n-2) (\res2 -> k (res1 + res2)))

------------------------------------------------------------------------------
-- | List functions

-- | CPS transform of the function `length`, which computes the length of a list
cpsLength :: [a] -> (Int -> r) -> r
cpsLength [] k = k 0 
cpsLength (x:xs) k = cpsLength xs (\res -> k (1 + res))

-- | CPS transform of the function `map`. The argument function (to be applied
--   every element of the list) is written in direct style
cpsMap :: (a -> b) -> [a] -> ([b] -> r) -> r
cpsMap f [] k = k []
cpsMap f (x:xs) k = cpsMap f xs (\res -> k ((f x):res))
------------------------------------------------------------------------------
-- Merge Sort

-- | Sort a list using mergeSort
-- mergeSort :: [Int] -> [Int]

-- | Split a list into two lists. All list elements in even indices
-- is placed in one sub-list, and all list elements in odd indices
-- is placed in the second sub-list.
-- split :: [Int] -> ([Int], [Int])

-- | Merge two sorted lists together
-- merge :: [Int] -> [Int] -> [Int]

-- | CPS transform of mergeSort
cpsMergeSort :: [Int] -> ([Int] -> r) -> r
cpsMergeSort [] k = k []
cpsMergeSort [x] k = k [x]
cpsMergeSort lst k = cpsSplit lst (\res1 -> cpsMergeSort (fst res1) (\res2 -> cpsMergeSort (snd res1) (\res3 -> cpsMerge res2 res3 k)))

-- | CPS transform of split
cpsSplit :: [Int] -> (([Int], [Int]) -> r) -> r
cpsSplit [] k = k ([],[])
cpsSplit [x] k = k ([x],[])
cpsSplit (x:xs:xxs) k = cpsSplit xxs (\res -> k (x:(fst res), xs:(snd res)))


-- | CPS transform of merge
cpsMerge :: [Int] -> [Int] -> ([Int] -> r) -> r
cpsMerge [] lst2 k = k lst2
cpsMerge lst1 [] k = k lst1
cpsMerge (x:xs) (y:ys) k =  if x > y
    then cpsMerge (x:xs) ys (\res -> k (y:res))
    else cpsMerge xs (y:ys) (\res -> k (x:res))

------------------------------------------------------------------------------
-- * Main Task. CPS Transforming The Yumyscript Interpreter *
------------------------------------------------------------------------------

-- | A CPS interpreter `cpsEval` for Yumyscript, which takes an environment,
--   an expression, and a continuation, and calls the continuation with
--   the evaluated value.
--   Notice that the type signature of `cpsEval` is less general compared to
--   usual, i.e. it is not:
--      Expr -> Env -> (Value -> r) -> r
--   This restriction on the type of the continuation makes it easier
--   to check for errors.
cpsEval :: Expr -> Env -> (Value -> Value) -> Value
cpsEval (Literal v) env k = k v

cpsEval (Lambda params body) env k = k $ constructClosure params body env

cpsEval (Plus a b) env k =  cpsEval a env (\res1 -> cpsEval b env (\res2 -> case(res1, res2) of
    (Num a, Num b) -> k (Num (a + b))
    _              -> Error "Plus"
    ))

cpsEval (Times a b) env k =  cpsEval a env (\res1 -> cpsEval b env (\res2 -> case(res1, res2) of
    (Num a, Num b) -> k (Num (a * b))
    _              -> Error "Times"
    ))

cpsEval (Equal a b) env k = cpsEval a env (\res1 -> cpsEval b env (\res2 -> case(res1, res2) of
    (x, y) -> if x == y
        then k T
      else k F
    ))

cpsEval (If cond expr alt) env k = cpsEval expr env (\ex -> cpsEval alt env (\al -> cpsEval cond env (\cd -> case (cd) of
    (T) -> k ex
    _   -> k al
    )))

cpsEval (Var name) env k = case (Data.Map.lookup name env) of
    Just a  -> k a
    Nothing -> Error "Var"

cpsEval (App fn args) env k = helpm args env (\res1 -> cpsEval fn env (\res2 -> case (res2) of
    Closure (Proc f) -> f res1 k
    _                -> Error "App"
    ))

cpsEval (Shift str exp) env k =  cpsEval exp (Data.Map.insert str ( Closure $ Proc $ \[vargs] c -> c $ k $ vargs ) env) id


-- cpsEval (Shift str exp) env k = cpsEval exp (Data.Map.insert str (Closure $ Proc $ k)) k
-- example4 = (cpsEval (Plus (Literal $ Num 2) ---k =(+ 2 _)
--             (Shift "d" (Plus (App (Var "d") [Literal $ Num 5])
--                (App (Var "d") [Literal $ Num 10]))
--                                                      ))
--                      emptyEnv
--                      id)

-- cpsEval (Reset exp) env k  =

helpm [] env k = k []
helpm (x:xs) env  k = cpsEval x env (\res1 -> helpm xs env (\res2 -> k (res1 : res2)))


-- Helper function to construct closures.
-- In our closure representation the information that was explicitly stored
-- in the A2 closure representation is be stored implicitly in an instance
-- of HaskellProc. A HaskellProc is constructed using the "Proc" value constructor.
-- The argument to "Proc" is a function with the type signature
--    [Value] -> (Value -> Value) -> Value
-- The first argument is the same as in A3Yumyscript.hs, and the second argument
-- represents the **continuation** to be evaluated *after* the closure is called.
--
-- When the closure is called (i.e. when the Yumyscript function is applied),
-- then a few things could happen:
--       * if the number of arguments in vargs is **the same** as the number of
--         parameters, then we wish to apply the body. The continuation `k` should
--         be applied after the body is evaluated, so `k` is passed in as a parameter
--         to the recursive call to `eval`
--       * if the number of arguments in vargs **less than** the number of
--         parameters, then we need to "return" a closure. We do so by passing
--         the closure to the continuation `k`
constructClosure :: [String] -> Expr -> Env -> Value
constructClosure params body env = Closure $ Proc $ \vargs k ->
    if length params == length vargs
    then let paramArgTuples = zip params vargs
             newEnv = foldl (\e (param, arg) -> Data.Map.insert param arg e)
                            env
                            paramArgTuples
          in cpsEval body newEnv k
    else if length params > length vargs
        then let paramAssigned = take (length vargs) params
                 paramRemaining = drop (length vargs) params
                 paramArgs = zip paramAssigned vargs 
                 newEnv = foldl (\e (param, arg) -> (Data.Map.insert param arg e)) env paramArgs
             in k $ constructClosure paramRemaining body newEnv
    else Error "App"

