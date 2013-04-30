{-# OPTIONS_GHC -O2 -optc-O2 #-}

--solve = reverse $ take 10 $ map (`mod` 10) $ takeWhile (>0) $ iterate (`div`10) (28433*(2^7830457) + 1)

solve = (28433 * (2^7830457) + 1) `mod` 10000000000

main = do putStrLn $ show solve