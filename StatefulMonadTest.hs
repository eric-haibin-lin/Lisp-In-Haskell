import Stateful hiding (Stateful, evaluate)
import Base
import StatefulMonad 
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
     "x = @x - @x;"++
     "y = @y / @x;" ++ "@y")

main = do
  test "evaluate" execute t1
  test "evaluate" execute t2