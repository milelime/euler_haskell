multiplesOfThreeOrFive :: Integer -> Integer
multiplesOfThreeOrFive n = sum [x | x <- [0 .. n - 1], mod x 3 == 0 || mod x 5 == 0]
