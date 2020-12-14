import Debug.Trace
import Data.Maybe

ex :: IO [Int]
ex = map read  . lines <$> readFile "day9inp"

main = ex >>= putStrLn . check
check :: [Int] -> String
check xs = show $ head weakness + last weakness
	where foldFunc (a,b) x 
		| length b < 25 = trace ("preamble") (a,x:b)
		| otherwise = ((x,checkElement 25 x b):a,x:b)
	      firstNum =  head . dropWhile snd . reverse . fst . foldl foldFunc ([],[]) $ xs
	      weakness = quicksort $  head $ (findTotalSequences (fst firstNum) xs)

findTotalSequences :: Int -> [Int] -> [[Int]]
findTotalSequences num (x:xs) = case findSequence num (x:xs) of
					Just x -> x: findTotalSequences num xs
					_ -> findTotalSequences num xs
findTotalSequences _ [] = []
quicksort :: Ord a => [a] -> [a]
quicksort (x:xs) = smaller ++ [x] ++ bigger
	where smaller = quicksort $ filter (<x) xs
	      bigger = quicksort $ filter (>=x) xs
quicksort [] = []



checkElement range num xs =  (>0) . length .  filter id $ do
		a <- prevEls
		b <- prevEls
		
		return $ a /= b && (a+b) == num
	where prevEls = take range xs

findSequence :: Int -> [Int] -> Maybe [Int]
findSequence num xs = recurseFunc 0 xs []
	where recurseFunc total (x:ys) stack
		| total > num = Nothing
		| total == num = Just stack
		| otherwise = recurseFunc (total+x) ys (x:stack)
	      recurseFunc total [] stack
		| total == num = Just stack
		| otherwise = Nothing
