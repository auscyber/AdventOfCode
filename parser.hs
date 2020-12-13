{-# LANGUAGE LambdaCase #-}
import  Data.Bifunctor 
import Control.Applicative
import Data.Char as C
import Debug.Trace
import qualified Data.Set as S

newtype Parser a = Parser {runParser :: String -> Maybe (a,String) }

instance Functor Parser where
	fmap f (Parser p) = Parser $ fmap (first f) . p

instance Applicative Parser where
	pure a = Parser $ \s -> Just  (a,s)
	(Parser a) <*> (Parser b) = Parser $ \s -> do
			(f,rest) <- a s
			fmap (first f) (b rest)

instance Alternative Parser where
	empty = Parser $ const Nothing
	(Parser a) <|> (Parser b)  = Parser $ \s -> a s <|> b s



parseChar :: Char -> Parser Char
parseChar c = Parser $ \case {(x:xs) -> if  x ==c then Just (c,xs) else Nothing; [] -> Nothing}

parseString :: String -> Parser String
parseString s = traverse parseChar s 


pads :: Parser String
pads = many (parseCharIf (==' '))

parseWord :: Parser String
parseWord = pads *> some (parseCharIf (/=' '))

parseInt :: Parser Int
parseInt = read <$> some (parseCharIf (C.isDigit))

parseCharIf :: (Char -> Bool) -> Parser Char
parseCharIf p = Parser $ \x -> case x of { (x:xs) -> if p x then Just (x,xs) else Nothing; [] -> Nothing}

type Question = Char
parseRow :: Parser [Char]
parseRow = some (parseCharIf (/='\n')) 

parseGroup :: Parser [[Char]]
parseGroup = many (  (\x -> trace (show x) x)<$> (parseRow <* parseChar '\n'))


parseAll :: Parser [[[Char]]]
parseAll = many (parseGroup <* parseChar '\n') <**> ((:) <$> parseGroup)

main = do
	Just (s,_) <- fmap (runParser parseAll) $ readFile "day6inp"  
	print $ foldr (+) 0 $ map ( length . S.toList . S.fromList . concat) s 
	print $ foldr (+) 0 $ map (totalElems) s

totalElems :: [[Char]] -> Int
totalElems xs = length $ filter (id) $ map (\e -> and $ map (e`elem`) xs) set
	where set = S.toList . S.fromList . concat$ xs
