module Ch3 where

main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where secondGreeting = (++) "hello" ((++) " " "world!")
  

area d = pi*(r*r) 
         where r = d/2

myGreeting :: String
myGreeting = (++) "hello" " world!"

thirdLetter :: String -> Char
thirdLetter x = x !! 3
