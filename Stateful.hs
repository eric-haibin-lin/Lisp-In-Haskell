module Stateful where

import Prelude hiding (LT, GT, EQ, id)
import Base
import Data.Maybe

data Value = IntV  Int
           | BoolV Bool
           | ClosureV String Exp Env
           | AddressV Int -- new        
  deriving (Eq, Show)


data Exp = Literal   Value
         | Unary     UnaryOp Exp
         | Binary    BinaryOp Exp Exp
         | If        Exp Exp Exp
         | Variable  String
         | Declare   String Exp Exp
         | Function  String Exp
         | Call      Exp Exp
         | Seq       Exp Exp     
         | Mutable   Exp         -- new
         | Access    Exp         -- new
         | Assign    Exp Exp   -- new
  deriving (Eq, Show)

type Memory = [Value]

-- Memory operations

access :: Int -> Memory -> Value
access n m = m !! n

update :: Int -> Value -> Memory -> Memory
update n v m = 
  let (before,(_:after)) = splitAt n m in
  before ++ [v] ++ after 

-- Show how memory works: see mul10



-- Modelling stateful computation

type Stateful t = Memory -> (t, Memory)
  
type Env = [(String, Value)]

evaluate :: Exp -> Env -> Stateful Value

evaluate (Literal v) env mem    = (v, mem)

evaluate (Unary op a) env mem   =
  let (av, mem') = evaluate a env mem in
    (unary op av, mem')

--BEGIN:Sema27
evaluate (Binary op a b) env mem =
  let (av, mem') = evaluate a env mem in
    let (bv, mem'') = evaluate b env mem' in
      (binary op av bv, mem'')
--END:Sema27

evaluate (If a b c) env mem =
  let (BoolV test, mem') = evaluate a env mem in
    evaluate (if test then b else c) env mem'

evaluate (Variable x) env mem = (fromJust (lookup x env), mem)

evaluate (Declare x e body) env mem =
  let (ev, mem') = evaluate e env mem
      newEnv = (x, ev) : env
  in
    evaluate body newEnv mem'

evaluate (Function x body) env mem = (ClosureV x body env, mem)

evaluate (Call f a) env mem  =
  let (ClosureV x body closeEnv, mem') = evaluate f env mem
      (av, mem'') = evaluate a env mem'
      newEnv = (x, av) : closeEnv
  in
      evaluate body newEnv mem''
--END:Summ9 BEGIN:Summ11 BEGIN:Sema20
evaluate (Mutable e) env mem =
  let (ev, mem') = evaluate e env mem in
    (AddressV (length mem'), mem' ++ [ev])
--END:Sema20

--BEGIN:Sema23
evaluate (Access a) env mem =
  let (AddressV i, mem') = evaluate a env mem in
      (access i mem', mem')
--END:Sema23

--BEGIN:Sema25
evaluate (Assign a e) env mem =
  let (AddressV i, mem') = evaluate a env mem in
    let (ev, mem'') = evaluate e env mem' in
      (ev, update i ev mem'')

evaluate (Seq e1 e2) env mem =
  let (_,mem') = evaluate e1 env mem in
  evaluate e2 env mem'

--END:Sema25
--END:Summ11



execute exp = v
  where (v, _) = evaluate exp [] []
  
-- same as in IntBool.hs
data BinaryOp = Add | Sub | Mul | Div | And | Or
              | GT | LT | LE | GE | EQ
  deriving (Eq, Show)

data UnaryOp = Neg | Not
  deriving (Eq, Show)

unary Not (BoolV b) = BoolV (not b)
unary Neg (IntV i)  = IntV (-i)

binary Add (IntV a)  (IntV b)  = IntV (a + b)
binary Sub (IntV a)  (IntV b)  = IntV (a - b)
binary Mul (IntV a)  (IntV b)  = IntV (a * b)
binary Div (IntV a)  (IntV b)  = IntV (a `div` b)
binary And (BoolV a) (BoolV b) = BoolV (a && b)
binary Or  (BoolV a) (BoolV b) = BoolV (a || b)
binary LT  (IntV a)  (IntV b)  = BoolV (a < b)
binary LE  (IntV a)  (IntV b)  = BoolV (a <= b)
binary GE  (IntV a)  (IntV b)  = BoolV (a >= b)
binary GT  (IntV a)  (IntV b)  = BoolV (a > b)
binary EQ  a         b         = BoolV (a == b)

{-
access :: Int -> [a] -> a
access i mem = mem !! i

update :: Int -> Value -> Memory -> Memory
update addr val mem =
  let (before, _:after) = splitAt addr mem in
    before ++ [val] ++ after
-}

{-

-}