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
keys = M.fromList $ [("cid",const True)
        ,("byr",\x -> read x `elem` [1920..2002])
        ,("iyr",\x -> read x `elem` [2010..2020])
        ,("eyr",\x -> read x `elem` [2020..2030])
        ,("hgt",\x -> case break (\y -> isNothing (readMaybe [y] :: Maybe Int)) x of {
                    (v,"cm") ->  read v `elem` [150..193];
                    (v,"in") ->  read v `elem` [59..76];
                    _ -> False;})
        ,("hcl", \x ->  head x == '#' 
                    && length (tail x) == 6 
                    && (and $ map (`elem` (['a'..'f'] `union` ['0'..'9']) ) (tail x )))
        ,("ecl",\x -> x `elem` ["amb","blu","gry","brn","grn","hzl","oth"])
        ,("pid", \x -> length x == 9 && (and $ map (`elem` ['0'..'9']) x ))]
type Passport = M.HashMap String String
newtype Parser s a b = Parser {runParser :: (s,a) -> (s,b) }


main = do
    f <- readFile "day4inp"
    print $ length $ run f


prune :: [Passport] -> [Passport]
prune = filter (\x ->  and $ map (flip M.member x) (filter (\a -> a/="cid") $ M.keys $ keys))  

parseKey :: String -> Maybe (String,String)
parseKey x
    | key `elem` M.keys keys = let Just b = M.lookup key keys in if b val  then Just (trace (show inp) inp ) else  Nothing
    | otherwise = Nothing
    where inp@(key,val) = let (key,_:val) = break (==':') x in (key,val)


run = prune . fold
    where fold :: String -> [Passport]
          fold [] = []
          fold inp' = let (s',p) = runParser splitChunk (inp',()) in if isJust p then fromJust p : fold s' else fold s'

splitChunk :: Parser String a (Maybe (M.HashMap String String))
splitChunk = Parser $ \(s,_) -> let (rs,x) = getChunk (s,[]) in (rs,M.fromList <$> traverse parseKey (words x))
    where getChunk (a,xs) = case a of
                            ('\n':'\n':xss) -> (xss,xs)
                            ('\n':xss) -> getChunk (xss, xs ++ [' '])
                            (b:xss) -> getChunk (xss,xs ++ [b])
                            [] -> ([],xs)


