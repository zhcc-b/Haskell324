{-|
Module: A2
Description: Assignment 2
Copyright: (c) University of Toronto Mississagua
               CSC324 Principles of Programming Languages, Fall 2022
-}
-- This lists what this module exports. Don't change this!
module A2
  (
    runYumy,
    eval
  )
where

-- You *may not* add imports from Data.Map, or any other imports
import A2Types(Expr(..), Value(..), Env)
import qualified Data.Map (lookup, insert, empty)


-- | Runs a Yumyscript expression by calling `eval` with the empty environment
runYumy :: Expr -> Value
runYumy e = eval e Data.Map.empty 


-- | An interpreter for the Yumyscript language.
eval :: Expr -> Env -> Value
eval (Literal v) env = v
eval (Plus a b) env = case ((eval a env), (eval b env)) of
    (Num x, Num y) -> (eval (Literal $ Num (x + y)) env) -- todo
    (Error x, Num y) -> Error x
    (Num x, Error y) -> Error y
    _              -> Error "Plus" -- todo
    -- what other patterns are missing above?
eval (Times a b) env = case ((eval a env), (eval b env)) of
    (Num x, Num y) -> (eval (Literal $ Num (x * y)) env)
    (Error x, Num y) -> Error x 
    (Num x, Error y) -> Error y
    _              -> Error "Times"
-- todo: handle Equal and If
eval (Equal a b) env = case ((eval a env), (eval b env)) of
    (Num x, Error y) -> Error y
    (Error x, Num y) -> Error x 
    (x, y) -> if x == y
        then T
      else F

eval (If cond expr alt) env = case ((eval cond env)) of
    (Error cond) -> Error cond
    (T) -> (eval expr env)
    _   -> (eval alt env)

eval (Var name) env  = case (Data.Map.lookup name env) of
    -- _       -> Closure ["x"] (Var "y") env
    Just a  -> (eval (Literal $ a) env) -- "a" is of type Value 
    Nothing -> Error "Var" -- "name" is not found in "env"
-- todo: handle Lambda and App

eval (Lambda p b) env = Closure p b env-- todo

eval (App fnExpr argExprs) env = case ((eval fnExpr env), (map (\x -> eval x env) argExprs)) of
    (Closure param body env, y) -> if (chek y) /= T
      then chek y
        else if (length param) > (length y)
          then Closure (drop (length y) param) body (updae env (zip param y))
        else if (length param) < (length y)
          then Error "App"
        else eval body (updae env (zip param y))


updae :: Env -> [(String, Value)] -> Env
updae env [] = env
updae env ((p,a):xs) = updae (Data.Map.insert p a env) xs

chek :: [Value] -> Value
chek [] = T
chek (x:xs) = case (x, xs) of
  (Error a, xs) ->  Error a
  (x, [])       ->  T
  (x, xs)    -> chek xs

