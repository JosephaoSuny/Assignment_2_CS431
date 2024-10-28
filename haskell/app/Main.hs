module Main where
import qualified GHC.List as List
import qualified GHC.Real as Int

-- list :: a -> [a]
list a = [a]

divisorsHelp positive current accumulator = do
    if current == 0 then accumulator
    else if positive `mod` current == 0 then divisorsHelp positive (current-1) (current : accumulator)
    else divisorsHelp positive (current-1) accumulator


divisors positive = divisorsHelp positive positive []

isPrime num = divisors num == [1, num]

primes = filter isPrime [1..maxBound :: Int]

perfs n = [x | x <- [2..n], List.sum (List.takeWhile (<x) (divisors x)) == x ]

main :: IO ()
main = do
    print (divisors 6)
    print (primes!!2000)
    print (perfs 28)

