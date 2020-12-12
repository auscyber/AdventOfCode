import System.IO
import Control.Monad.State





main = do
    a <- slope 3 1
    b <- slope 1 1
    c <- slope 5 1
    d <- slope 7 1
    e <- slope 1 2
    print $ a*b*c*d*e




slope a b = length . filter id . line a b 0 0  . convertTree <$> input

input :: IO String
input = openFile "day3inp.txt" ReadMode >>= hGetContents

countTrees :: String -> Int
countTrees = foldr (\x acc ->if x == '#' then acc+1 else acc) 0


convertTree  = map (concat. repeat . map replaceTree) . lines 


replaceTree '#' = True
replaceTree _ = False

line :: Int -> Int -> Int -> Int -> [[Bool]] -> [Bool]
line l r lin col xs 
    | lin >= length xs = []
    | otherwise = xs !! lin !! col : line l r (lin+r) (col+l) xs
