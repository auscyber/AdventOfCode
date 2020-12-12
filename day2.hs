import System.IO


main = do
    dat <- openFile "day2inp" ReadMode >>= hGetContents
    print . length $ lines dat >>= parse . words
    print . length $ lines dat >>= parse2 . words


parse :: [String] -> [String]
parse (count:(letter:_):pass:_) 
    | letter `elem` pass = if c `elem` count' then [pass] else []
    | otherwise = []
    where count' = let (start,finish) = parseCount count in [start,finish]
          c = foldr (\x acc -> if x == letter then acc+1 else acc) 0 pass


parseCount ::  String -> (Int,Int)
parseCount count = let (start,_:finish) = break (=='-') count in (read start,read finish)

    
parse2 :: [String] -> [String]
parse2 (count:(letter:_):pass:_)
    | letter `elem` pass = countInPass'
    | otherwise = []
    where (start,finish) = let (start,finish) = parseCount count in (start-1,finish-1)
          countInPass'
                | finish > length pass || start > length pass = []
                | eq start == not ( eq finish) = [pass]
                | otherwise  = []
                where eq index = pass !! index == letter
