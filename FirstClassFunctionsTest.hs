import Base
import Prelude hiding (LT, GT, EQ, repeat, iterate, zipWith)
import FirstClassFunctions
import FirstClassFunctionsParse

-- Javascript

p2 = parseExp ("var absolute = function(x) {if (x > 0) x; else -x}; absolute(3)") 

p3 = parseExp ("var absolute = function(x) {if (x > 0) x; else -x}; absolute(-3)") 

p4 = parseExp ("var x = 2; var f = function(y) {x + y}; var x = 3; f(5)")

p5 = parseExp ("var max = function(x) {function(y) {if (x > y) x; else y}}; max(2)(3)")


-- recursive definitions

fact = parseExp ("var fact = function(n){if (n == 0) 1; else n * fact(n-1)}; fact(10)")

fact2 = parseExp ("var fact = function(n){if (n == 0) 1; else n * fatn(n-1)}; fact(10)")

fib = parseExp ("var fib = function(n){if (n == 0) 0; else (if (n==1) 1; else fib(n-1) + fib(n-2))}; fib(10)")

repMin' :: ([Int],Int) -> (Int, [Int])
repMin' ([],r)    = (1000, []) -- lets assume maximum number is 1000
repMin' (x:xs,r)  = let (m,rs) = repMin' (xs,r) in (x `min` m, r:rs) 

repMin :: [Int] -> [Int]
repMin xs = let (u,ys) = repMin' (xs,u) in ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith f xs     ys     = []

repeat :: a -> [a]
repeat n = let list = n : list in list

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibN :: Int -> Int
fibN n = fibs !! n

multiple (x:xs) n = 
   if (x*x > n) then False
   else if (n `mod` x == 0) then True
   else multiple xs n

primes = 2 : filter (\n -> not (multiple primes n)) [3..]
