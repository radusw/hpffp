module Learn where

sayHi :: String -> IO ()
sayHi x = putStrLn ("Hi, " ++ x ++ "!")

circleArea :: Double -> Double
circleArea r = pi * (r * r)

waxOn z = x * 5
  where x = y ^ 2
        y = z + 8
waxOff x = triple x
triple x = x * 3

foo x = 
  let y = x * 2
      z = x ^ 2
  in 2 * y * z


printInc n = print plusTwo where plusTwo = n + 2

printInc2 n = let plusTwo = n + 2
              in print plusTwo

printInc2' n = (\plusTwo -> print plusTwo) (n + 2)

p1 = let x = 7; y = negate x; z = y * 10 in z / x + y
p2 = z / x + y where x = 7; y = negate x; z = y * 10 
p3 = (\z -> \y -> \x -> z / x + y) ((\a -> a * 10) ((\b -> negate b) 7)) ((\m -> negate m) 7) 7
p4 = (\q -> (\z -> \y -> \x -> z / x + y) ((\a -> a * 10) ((\b -> negate b) q)) ((\m -> negate m) q) q) 7
p5 = (\x -> (negate x) * 10 / x + (negate x)) 7
