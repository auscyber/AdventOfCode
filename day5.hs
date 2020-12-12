import Debug.Trace
data Position = F | B | L | R deriving (Show,Eq)
type Seat = (Int,Int,Int)

parseRow :: Position -> [Int] -> [Int]
parseRow F xs  =  fst . splitAt (length xs `div` 2 ) $ xs
parseRow B xs =   snd . splitAt (length xs `div` 2 ) $ xs
parseRow L xs =  fst . splitAt (length xs `div` 2 ) $ xs
parseRow R xs =   snd . splitAt (length xs `div` 2 ) $ xs

traceS :: Show a => a -> a
-- traceS = (\x -> trace (show x) x)  
traceS = id
main = do
   f <- readFile "day5inp"
   print $ mySeat f 
mySeat f = filter (not . (`elem`ls))  alls
	where ls = list f
	      alls = allSeats ls

list f = map (getSeatID .  parse . convertRow) $ lines f


bignsmall xs = (last sorted, head sorted )
	where sorted = quickSort xs
allSeats :: [Int] -> [Int]
allSeats xs = let (big,small) = bignsmall xs in [small..big]


getSeatID (_,_,a) = a

quickSort (x:xs) = smaller' ++ [x] ++ bigger'
	where smaller' = quickSort $ filter (<x) xs
	      bigger' = quickSort $ filter (>=x) xs 
quickSort [] = []

parse :: [Position] -> Seat
parse xs = (row,col,seatID )
	where row = head . foldl (\acc x-> parseRow x acc ) [0..127] $ traceS $ take 7 xs
	      col = head . foldl (flip parseRow) [0..7]  $ traceS $  snd $ splitAt 7 xs 
	      seatID = row * 8 + col
convertRow :: String -> [Position]
convertRow ('F':xs) = F:convertRow xs
convertRow ('B':xs) = B:convertRow xs
convertRow ('L':xs) = L:convertRow xs
convertRow ('R':xs) = R:convertRow xs
convertRow [] = []
