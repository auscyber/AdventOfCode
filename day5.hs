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
mySeat f = filter (\(_,a) -> not a) $ map (\x -> (x,x`elem` ls)) alls
	where ls = list f
	      alls = allSeats ls

list f = map (getSeatID .  parse . convertRow) $ lines f


bignsmall xs = (head $ quickSort False xs,head $ quickSort True xs)

allSeats :: [Int] -> [Int]
allSeats xs = let (big,small) = bignsmall xs in [small..big]


getSeatID (_,_,a) = a

quickSort b (x:xs) 
	| b = smaller' ++ [x] ++ bigger'
	| otherwise = bigger' ++ [x] ++ smaller'
	where smaller' = quickSort b $ filter (<x) xs
	      bigger' = quickSort b $ filter (>=x) xs 
quickSort b [] = []

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
