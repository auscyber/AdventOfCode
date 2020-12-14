import Parser
import Control.Applicative
import Debug.Trace
import qualified Data.Set as S
import Control.Monad
import qualified Data.List as L
import Data.Maybe
data Bag = Bag String String deriving (Show,Ord)
instance Eq Bag where
	(Bag a b) == (Bag c d) = a == c && b == d


data Rule = Rule Bag [(Int,Bag)] deriving Show

extractBags :: [(Int,Bag)] -> [Bag]
extractBags = map (\(_,x ) -> x) 
ex = "light red bags contain 1 bright white bag."

parseWordIgnoreChar :: [Char] -> Parser String
parseWordIgnoreChar c = recurseCheck <$> parseWord
	where   recurseCheck [] = []
		recurseCheck xs = if last xs `elem` c then recurseCheck $ fst $ splitAt (length xs -1) xs else xs
r = runParser


parseRule :: Parser Rule
parseRule = Rule <$> parseBag
	<*> ((pads *> parseString "contain" *> ignoreChar 's' *> ignoreChar ',' <* pads) 
	*> ((many (((,) <$> parseInt <*> parseBag <* pads) ))  
	<|> ([] <$ parseString "no other bags" <* pads ) ))  

parseBag :: Parser Bag
parseBag = Bag <$> ( parseWord) <* pads <*> parseWordIgnoreChar "s," <* pads <* parseWordIgnoreChar "s,"
mine = Bag "shiny" "gold"

main = do
	f <- readFile "day7inp"
	print $ fromJust $ instances <$>  (traverse ( fmap fst .  runParser parseRule) $ lines f)

instances xs = length $ recur [mine]
	where recur ls = let ts =   foldr (L.union) [] $  map (\b ->  b:findIn xs b ) ls in  if ts == ls then ls else recur ts

findIn xs el = getSet [r | (Rule r ab) <- xs, (_,l) <- ab, l == el   ]

getSet ::(Show a, Eq a) => [a] -> [a]
getSet = foldr (\x acc -> if x `elem` acc then   acc else x:acc) [] 
