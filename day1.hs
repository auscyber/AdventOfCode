import System.IO
import Control.Monad
main = do
    f <- openFile "day1inp" ReadMode
    lin <- hGetContents f >>= return . map read . lines
    print $ parse1 lin
    print $ parse2 lin
    return ()


parse1 :: [Integer] -> [Integer]
parse1 l = do
    a <- l
    b <- l
    guard (a+b == 2020)
    return (a*b)


parse2 :: [Integer] -> [Integer]
parse2 l = do
    a <- l
    b <- l
    c <- l
    guard (a+b+c==2020)
    return (a*b*c)
