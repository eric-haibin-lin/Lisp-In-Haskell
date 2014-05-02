module StatefulCheckedMonad where

import Prelude hiding (LT, GT, EQ, id)
import Base
import Data.Maybe
import Stateful hiding (Stateful, evaluate, unary, binary)

data Checked a = Good a | Error String
  deriving Show

data CheckedStateful t = CST (Memory -> (Checked t, Memory))

--BEGIN:StatefulMonad1
--data Stateful t = ST (Memory -> (t, Memory))
--END:StatefulMonad1

--BEGIN:StatefulMonad2
instance Monad CheckedStateful where
  return val = CST (\m -> (Good val, m))
  (CST c) >>= f = 
    CST (\m -> case c m of 
        (Error msg, m') -> (Error msg, m')
        (Good v, m') -> let CST f' = f v in f' m')
--END:StatefulMonad2
     
--BEGIN:StatefulMonad3
evaluate :: Exp -> Env -> CheckedStateful Value
-- basic operations
evaluate (Literal v) env = return v
evaluate (Unary op a) env = do
  av <- evaluate a env
  checked_unary op av
evaluate (Binary op a b) env = do
  av <- evaluate a env
  bv <- evaluate b env
  checked_binary op av bv
evaluate (If a b c) env = do
  BoolV cond <- evaluate a env
  evaluate (if cond then b else c) env

-- variables and declarations
evaluate (Declare x e body) env = do    -- non-recursive case
  ev <- evaluate e env
  let newEnv = (x, ev) : env
  evaluate body newEnv
evaluate (Variable x) env = 
  case lookup x env of
    Nothing -> return_error ("Variable " ++ x ++ " undefined")
    Just v -> return v

-- first-class functions
evaluate (Function x body) env = 
  return (ClosureV  x body env)
evaluate (Call fun arg) env = do
  ClosureV  x body closeEnv <- evaluate fun env
  argv <- evaluate arg env
  let newEnv = (x, argv) : closeEnv
  evaluate body newEnv

-- mutation operations
evaluate (Seq a b) env = do
  evaluate a env
  evaluate b env
evaluate (Mutable e) env = do
  ev <- evaluate e env
  newMemory ev        
evaluate (Access a) env = do
  AddressV i <- evaluate a env
  readMemory i
evaluate (Assign a e) env = do
  AddressV i <- evaluate a env
  ev <- evaluate e env
  updateMemory ev i
  return ev
--END:StatefulMonad3

--BEGIN:StatefulHelper1
newMemory val = CST (\mem->  ( Good (AddressV (length mem)), mem ++ [val]))
--END:StatefulHelper1

--BEGIN:StatefulHelper2
readMemory i = CST (\mem-> (Good (access i mem), mem))
--END:StatefulHelper2

--BEGIN:StatefulHelper3
updateMemory val i = CST (\mem-> (Good (), update i val mem))
--END:StatefulHelper3


checked_unary :: UnaryOp -> Value -> CheckedStateful Value
checked_unary Not (BoolV b) = return (BoolV (not b))
checked_unary Neg (IntV i) = return (IntV (-i))
checked_unary _ _ =  return_error ("Type Error!")

checked_binary :: BinaryOp -> Value -> Value -> CheckedStateful Value
checked_binary Add (IntV a) (IntV b) = return (IntV (a + b))
checked_binary Sub (IntV a) (IntV b) = return (IntV (a - b))
checked_binary Mul (IntV a) (IntV b) = return (IntV (a * b))
checked_binary Div (IntV a) (IntV 0) = return_error ("Divide by zero")
checked_binary Div (IntV a) (IntV b) = return (IntV (a `div` b))
checked_binary And (BoolV a) (BoolV b) = return (BoolV (a && b))
checked_binary Or (BoolV a) (BoolV b) = return (BoolV (a || b))
checked_binary LT (IntV a) (IntV b) = return (BoolV (a < b))
checked_binary LE (IntV a) (IntV b) = return (BoolV (a <= b))
checked_binary GE (IntV a) (IntV b) = return (BoolV (a >= b))
checked_binary GT (IntV a) (IntV b) = return (BoolV (a > b))
checked_binary EQ a b = return (BoolV (a == b))
checked_binary _ _ _ = return_error ("Type Error!")

return_error :: String -> CheckedStateful Value
return_error e = CST (\m -> (Error e, m))
 


runStateful (CST c) = 
   let (val, mem) = c [] in val