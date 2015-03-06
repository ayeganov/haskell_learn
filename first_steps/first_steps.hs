
module Main where
import System.Environment
import Data.List
import Data.Bits

removeNonUpper :: [Char] -> [Char]
removeNonUpper st = [c | c <- st, c `elem` ['A'..'Z']]

{-fib' :: (Num n) => n -> n
fib' num
    | num <= 0 = 
-}
fib'':: Int -> Integer
fib'' n = fib' n 1 0
    where
        fib' cnt s p
            | cnt > 0 = fib' (cnt-1) (s + p) s
            | cnt == 0 = p

fib :: Int -> Integer
fib n = snd . foldl' fib' (1, 0) . dropWhile not $
            [testBit n k | k <- let s = bitSize n in [s-1,s-2..0]]
    where
        fib' (f, g) p
            | p         = (f*(f+2*g), ss)
            | otherwise = (ss, g*(2*f-g))
            where ss = f*f+g*g


main :: IO()
main = do
    args <- getArgs
    putStrLn ("Hello, " ++ removeNonUpper (args !! 0))
    putStrLn ("Fib num 100 is " ++ show (fib 20008))
    putStrLn ("Fib num 100 is " ++ show (fib'' 20008))
