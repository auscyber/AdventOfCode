{-# LANGUAGE Arrows #-}
import System.IO 
import qualified Control.Category as Cat
import Control.Arrow
import Control.Applicative
import qualified Data.HashMap.Lazy as M
import qualified Data.Bifunctor
import Debug.Trace

keys = ["cid","byr","iyr","eyr","hgt","hcl","ecl","pid"]
main = do
    f <- readFile "day4inp"
    let (_,chunk) = runParser run (f,[])
    print $ length chunk




prune :: Parser String [Passport] [Passport]
prune = arr $ \p -> filter (\x -> and $ fmap (flip M.member x) (tail keys) ) p

parseKey :: String -> Maybe (String,String)
parseKey x
    | key `elem` keys = Just inp
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

