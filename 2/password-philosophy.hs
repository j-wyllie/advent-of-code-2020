import System.IO
import Control.Monad
import Data.String

main :: IO ()
main = do
         handle <- openFile "input.txt" ReadMode
         contents <- hGetContents handle
         print $ getNumberOfValidPasswords contents
         hClose handle

getNumberOfValidPasswords :: String -> Int 
getNumberOfValidPasswords = length $ filter isValid $ lines 

isValid :: String -> Bool
isValid pw = numLetters () 
             where numLetters l = length $ filter (== l)

lowerLimit :: String -> Int
lowerLimit = takeWhile (/= '-')

upperLimit :: String -> Int
upperLimit = dropWhile (/= '-') $ takeWhile (/= ' ') 

ruleLetter :: String -> Char
ruleLetter = last $ takeWhile (/= ':')

password :: String -> String
password = dropWhile (== ' ') $ dropWhile (/= ':')  
