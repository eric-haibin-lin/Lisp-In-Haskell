import Stateful hiding (Stateful, evaluate, unary, binary)
import Base
import StatefulCheckedMonad 
import StatefulParse

t1 = parseExp ("var x = mutable 3;"++
     "var y = mutable true;"++
     "if (@y) { x = @x + 1 } else { x };"++
     "@x")

t2 = parseExp ("var x = mutable 3;"++
     "var y = mutable 7;"++
     "x = @x + @y;"++
     "y = @y * @x;" ++ "@y")

t3 = parseExp ("var x = mutable 3;"++
     "var y = mutable 7;"++
     "x = z")

t4 = parseExp ("var x = 3 + true;x")

t5 = parseExp ("var f = function(a) {if (a > 0) { -a*2 } else { -a } };"++
  "var x = mutable -2;"++
  "var y = mutable 3;"++
  "f(@x) + f(@y)")

main = do
  test "evaluate" execute t1
  test "evaluate" execute t3
  test "evaluate" execute t4
