import Data.List as L

reverseDigits :: Integer -> Integer
reverseDigits n =
  let str = show n
  in read $ reverse str

isNotPalindrome n =
  let str = show n
  in str /= reverse str

isLychrel n =
  50 == (length $ takeWhile isNotPalindrome $ take 50 $ tail $ iterate (\x -> x + reverseDigits x) n)

solve = length $ [n | n <- [1..10000], isLychrel n]

main =
  do putStrLn $ show solve
