{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
module Day4 (main) where
import System.IO 
import qualified Control.Category as Cat
import Control.Arrow
import Control.Applicative
import qualified Data.HashMap as M
import qualified Data.Bifunctor
import Debug.Trace
import Text.Read
import Data.Maybe
import Data.List
keys = M.fromList ak
ak :: [(String, String -> Bool)]
ak = [("cid",const True)
        ,("byr",\x -> (read x) >= 1920 && (read x) <= 2002)
        ,("iyr",\x -> (read x) >= 2010 && (read x) <= 2020)
        ,("eyr",\x -> (read x) >= 2020 && (read x) <= 2030)
        ,("hgt",\x -> case break (\y -> isNothing (readMaybe [y] :: Maybe Int)) x of {
                    (v,"cm") ->  read v >= 150 && 193 >= read v;
                    (v,"in") ->  read v >= 59 && 76 >= read v;
                    _ -> False;
})
        ,("hcl", \x ->  head x == '#' 
                    &&  length (tail x) == 6 
                    && length ( filter (`elem` (['a'..'f'] `union` ['0'..'9']) ) (tail x )) == 6 )
        ,("ecl",\x -> case x of {
                    "amb" -> True;
                    "blu" -> True;
                    "gry" -> True;
	            "brn" -> True;
                    "grn" -> True;
                    "hzl" -> True;
                    "oth" -> True;
		     _ -> False })
        ,("pid",\x -> length x == 9 && (and $ map (`elem` ['0'..'9']) x))]

main = do
    f <- readFile "day4inp"
    let (_,chunk) = runParser run (f,[])
    print $ length chunk




prune :: Parser String [Passport] [Passport]
prune = arr $ filter (\x ->  and $ map (flip M.member x) (filter (\a -> a/="cid") $ M.keys $ keys))  

parseKey :: String -> Maybe (String,String)
parseKey x
    | key `elem` M.keys keys = let Just b = M.lookup key keys in if b val  then Just (trace (show inp) inp ) else  Nothing
    | otherwise = Nothing
    where inp@(key,val) = let (key,_:val) = break (==':') x in (key,val)

newtype Parser s a b = Parser {runParser :: (s,a) -> (s,b) }


run :: Parser String [Passport] [Passport]
run = proc inp -> do
    i <- areCharsLeft -< ()
    if i then do 
        inp' <- combine -< inp
        run -< inp'
        else do
        prune -< inp

areCharsLeft = Parser $ \(s,_) -> (s,not . null $ s)

type Passport = M.HashMap String String

splitChunk :: Parser String a (Maybe (M.HashMap String String))
splitChunk = Parser $ \(s,_) -> let (rs,x) = getChunk (s,[]) in (rs,M.fromList <$> traverse parseKey (words x))
    where getChunk (a,xs) = case a of
                            ('\n':'\n':xss) -> (xss,xs)
                            ('\n':xss) -> getChunk (xss, xs ++ [' '])
                            (b:xss) -> getChunk (xss,xs ++ [b])
                            [] -> ([],xs)


instance Arrow (Parser s) where
   arr f = Parser $ Data.Bifunctor.second f
   first (Parser f) = Parser $ \(s,(a,b)) -> let (s',b') = f (s,a) in (s',(b',b))
instance ArrowChoice (Parser s) where
    left (Parser f) = Parser $ \(s,a) -> case a of
                                            Left a' -> let (s',b) = f (s,a') in (s',Left b)
                                            Right a' -> (s,Right a')
instance Cat.Category (Parser s) where
    id = arr id
    Parser f . Parser g = Parser $ f . g




combine :: Parser String [Passport] [Passport]
combine = Parser $ \(s,p) ->  case runParser splitChunk (s,()) of
                                    (s', Just outp) -> (s',outp:p)
                                    (s',Nothing) -> (s',p)

