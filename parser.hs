{-# LANGUAGE LambdaCase #-}
module Parser (Parser,parseChar,parseString,parseCharIf,parseInt,parseWord,pads,ignoreChar,ignoreCharIf,runParser,oneOf) where
import  Data.Bifunctor 
import Control.Applicative
import Data.Char as C
import Debug.Trace
import qualified Data.Set as S
import Data.Foldable
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

ignoreChar c = Parser $ \case {(x:xs) -> if x == c then Just (c,xs) else Just (c,x:xs); [] -> Nothing}
ignoreCharIf prd = Parser $ \case {x:xs -> if prd x then Just (x,xs) else Just (x,x:xs); [] -> Nothing}

parseString :: String -> Parser String
parseString s = traverse parseChar s 

pads :: Parser String
pads = many (parseCharIf (==' '))

parseWord :: Parser String
parseWord = pads *> some (parseCharIf (/=' '))

parseInt :: Parser Int
parseInt = read <$> some (parseCharIf (\c -> C.isDigit c || c=='-' ))

parseCharIf :: (Char -> Bool) -> Parser Char
parseCharIf p = Parser $ \x -> case x of { (x:xs) -> if p x then Just (x,xs) else Nothing; [] -> Nothing}

oneOf :: [Parser a] -> Parser a
oneOf = foldl' (<|>) empty
