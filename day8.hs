import Parser
import Data.Maybe
import Control.Applicative
import Debug.Trace
main = readFile "day8inp" >>= print . (mapReplace <$>) .   traverse (fmap fst . runParser parseIns) .  lines


mapReplace xs = head $ filter isJust $ map (replace xs) [0..(length xs + 1)]
replace :: [Instruction] -> Int -> Maybe Int
replace xs y  = 
	case splitAt y xs of
	    (ss,i:ts) ->
		case i of 
		    Nop x False | x/=0 -> runIns (ss++(Jmp x False:ts))
 		    Jmp x False -> runIns (ss++(Nop x False:ts))
	            b -> Nothing
            (ss,[]) -> Nothing

data Instruction = Acc Int Bool | Jmp Int Bool | Nop Int Bool deriving Show
parseNPInt :: Parser Int
parseNPInt = ignoreChar '+' *> parseInt

runIns t = moveInstruction 0 (t,[])

parseIns :: Parser Instruction
parseIns =  oneOf [Acc <$ parseString "acc" <* pads <*> parseNPInt <*> pure False
		,Jmp <$ parseString "jmp" <* pads <*> parseNPInt <*> pure False
		,Nop <$ parseString "nop" <* pads <*> parseNPInt <*> pure False]

moveInstruction :: Int -> ([Instruction],[Instruction]) -> Maybe Int
moveInstruction acc ((Acc x False):xs,ss) = 
	moveInstruction (acc+x) (xs,(Acc x True:ss))
moveInstruction acc ((Nop x False):xs,ss) =  
	moveInstruction acc (xs,(Nop x True:ss))
moveInstruction acc ((Jmp x False):xs,ss)
	|  x > 0 = case splitAt (x-1) xs of 
			(fs,l:ns) -> moveInstruction acc (l:ns, (reverse fs) ++ (Jmp x True):ss)
			_ -> Just acc
	|  x < 0 = case splitAt (negate (x+1)) ss of 
			(fs,l:ns) -> moveInstruction acc (l:(reverse fs) ++ (Jmp x True):xs ,ns)
			_ -> Just acc
moveInstruction acc ([],_) = Just acc
moveInstruction acc _ =  Nothing

ex = map (fromJust . fmap fst . runParser parseIns) ["nop 0","acc +1","jmp +4","acc +3","jmp -3","acc -99","acc +1","jmp -4","acc +6"]
bob :: Maybe Int -> String
bob x = case x of
	Just x | x > 10 -> "big 10" | x < 10 -> "smaller than 10"
	Nothing -> "lmao nothing"



tshow :: (a -> String) -> a -> a
tshow f a = trace (f a) a
