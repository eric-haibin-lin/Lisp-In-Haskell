{-# OPTIONS -XFlexibleInstances #-}

module ErrorChecking where

import Prelude hiding (LT, GT, EQ, id) -- , (>>=), return)
import FirstClassFunctions hiding (evaluate)

data Checked a = Good a | Error String
  deriving Show

evaluate :: Exp -> Env -> Checked Value
evaluate (Literal v) env = Good v
evaluate (Variable x) env =
  case lookup x env of
    Nothing -> Error ("Variable " ++ x ++ " undefined")
    Just v -> Good v
--END:Hand11 BEGIN:Hand15
evaluate (Unary op a) env =
  case evaluate a env of
    Error msg -> Error msg
    Good av -> checked_unary op av
evaluate (Binary op a b) env =
  case evaluate a env of
    Error msg -> Error msg
    Good av ->
      case evaluate b env of
        Error msg -> Error msg
        Good bv ->
          checked_binary op av bv

-- Monadic evaluation: take 1:

{-

{- Bind -}

(>>=) :: Checked a -> (a -> Checked b) -> Checked b
x >>= f = 
  case x of 
    Error msg -> Error msg
    Good v -> f v

return :: a -> Checked a
return v = Good v

evaluateM :: Exp -> Env -> Checked Value
evaluateM (Literal v) env = return v
evaluateM (Variable x) env = 
  case lookup x env of
    Nothing -> Error ("Variable " ++ x ++ " undefined")
    Just v -> return v
evaluateM (Unary op a) env = 
  evaluateM a env >>= checked_unary op
evaluateM (Binary op a b) env =
  evaluateM a env >>= 
    \v1 -> evaluateM b env >>= 
       \v2 -> checked_binary op v1 v2
-}

-- Monadic evaluation: take 2

instance Monad Checked where
   return v = Good v
   x >>= f = 
     case x of 
       Error msg -> Error msg
       Good v -> f v

evaluateM :: Exp -> Env -> Checked Value
evaluateM (Literal v) env = return v
evaluateM (Variable x) env = 
  case lookup x env of
    Nothing -> Error ("Variable " ++ x ++ " undefined")
    Just v -> return v
evaluateM (Unary op a) env = 
  do  v <- evaluate a env
      checked_unary op v
evaluateM (Binary op a b) env =
  do v1 <- evaluate a env
     v2 <- evaluate b env
     checked_binary op v1 v2

execute exp = evaluate exp []

executeM exp = evaluateM exp []

checked_unary :: UnaryOp -> Value -> Checked Value
checked_unary Not (BoolV b) = Good (BoolV (not b))
checked_unary Neg (IntV i) = Good (IntV (-i))
checked_unary _ _ =  Error "Type Error!"

checked_binary :: BinaryOp -> Value -> Value -> Checked Value
checked_binary Add (IntV a) (IntV b) = Good (IntV (a + b))
checked_binary Sub (IntV a) (IntV b) = Good (IntV (a - b))
checked_binary Mul (IntV a) (IntV b) = Good (IntV (a * b))
checked_binary Div (IntV a) (IntV 0) = Error "Divide by zero"
checked_binary Div (IntV a) (IntV b) = Good (IntV (a `div` b))
checked_binary And (BoolV a) (BoolV b) = Good (BoolV (a && b))
checked_binary Or (BoolV a) (BoolV b) = Good (BoolV (a || b))
checked_binary LT (IntV a) (IntV b) = Good (BoolV (a < b))
checked_binary LE (IntV a) (IntV b) = Good (BoolV (a <= b))
checked_binary GE (IntV a) (IntV b) = Good (BoolV (a >= b))
checked_binary GT (IntV a) (IntV b) = Good (BoolV (a > b))
checked_binary EQ a b = Good (BoolV (a == b))
checked_binary _ _ _ = Error "Type Error!"





