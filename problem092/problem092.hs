import qualified Data.IntMap as IMap

getDigits :: Integral b => b -> [b]
getDigits n = map (`mod` 10) . takeWhile (>0) $ iterate (`div` 10) n

sumSquares :: Integral a => a -> a
sumSquares n = foldl (\a x-> x^2 + a) 0 $ getDigits n

chain :: Integral a => a -> Bool
chain n = 89 == (head $ dropWhile (\x -> (x /= 1) && (x /= 89)) $ iterate (sumSquares) n)

solve =
  let tbl = IMap.fromList (map (\x -> (x, chain x)) [1..567]) in
  length $ filter (== True) $ [IMap.findWithDefault False (sumSquares n) tbl | n <- [1..10000000]]

main = do putStrLn $ show solve
