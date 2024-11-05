import Data.Array
import Data.List (group, sort, union)

pairwise :: (a -> a -> a) -> [a] -> [a]
pairwise f (xs : ys : t) = f xs ys : pairwise f t
pairwise _ t = t

primes :: [Int]
primes = 2 : _Y ((3 :) . gaps 5 . _U . map (\p -> [p * p, p * p + 2 * p ..]))
  where
    _Y g = g (_Y g)
    _U ((x : xs) : t) = x : (union xs . _U . pairwise union) t
    gaps k s@(x : xs)
        | k < x = k : gaps (k + 2) s
        | otherwise = gaps (k + 2) xs

factorize :: Int -> [Int]
factorize n = primeFactors n primes
  where
    primeFactors 1 _ = []
    primeFactors _ [] = []
    primeFactors m (p : ps)
        | m < p * p = [m]
        | r == 0 = p : primeFactors q (p : ps)
        | otherwise = primeFactors m ps
      where
        (q, r) = quotRem m p

primePowers :: Int -> [(Int, Int)]
primePowers n = [(head x, length x) | x <- group $ factorize n]

divisors :: Int -> [Int]
divisors n =
    filter (< n) $
        map product $
            sequence
                [take (k + 1) $ iterate (p *) 1 | (p, k) <- primePowers n]

upperBound :: Int
upperBound = 28123

abundant :: Int -> Bool
abundant n = (sum . divisors) n > n

abundantsArray :: Array Int Bool
abundantsArray = listArray (1, upperBound) $ map abundant [1 .. upperBound]

abundants :: [Int]
abundants = filter (abundantsArray !) [1 .. upperBound]

remainders :: Int -> [Int]
remainders x = map (x -) $ takeWhile (<= x `quot` 2) abundants

sums :: [Int]
sums = filter (any (abundantsArray !) . remainders) [1 .. upperBound]

main :: IO ()
main = print $ sum [1 .. upperBound] - sum sums
