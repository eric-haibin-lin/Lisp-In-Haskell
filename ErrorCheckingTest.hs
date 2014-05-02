import Base
import FirstClassFunctions hiding (evaluate, execute)
import FirstClassFunctionsParse
import ErrorChecking


testUV = execute (parseExp "x")

testDBZ = execute (parseExp "3 / 0")

testTE = execute (parseExp "3 + true")

testGood = executeM (parseExp "3 + 4")
