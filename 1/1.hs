import System.IO
import Control.Monad

main :: IO ()
main = do
         let list = []
         handle <- openFile "input.txt" ReadMode
         contents <- hGetContents handle
         let input = map (\x -> read x :: Integer) (lines contents)
         let pair = head $ filter (sumsToX 2020) (cartesianProduct input input)
         print $ fst pair * snd pair
         hClose handle

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys =  concat $ map (\x -> map (\y -> (x, y)) ys) xs

sumsToX :: (Num a, Eq a) => a -> (a, a) -> Bool
sumsToX x pair = fst pair + snd pair == x


