import RainbowAssign
import qualified Data.Map as Map
import Data.Maybe



  
pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 8            -- length of each password
nLetters = 5            -- number of letters to use in passwords: 5 -> a-e
width = 40              -- length of each chain in the table
height = 1000           -- number of "rows" in the table
filename = "table.txt"  -- filename to store the table


pwReduce :: Hash -> Passwd
pwReduce hash_ = map toLetter (reverse (take pwLength (helper (fromEnum hash_))))
    where
        helper :: Int -> [Int]
        helper 0 = []
        helper hash = [hash `mod` nLetters] ++ helper (hash `div` nLetters)

getHashes :: [Passwd] -> [Hash]
getHashes passwords = map pwHash passwords

listToTuple :: [Hash] -> [Passwd] -> [(Hash, Passwd)]
listToTuple [] [] = []
listToTuple [] (_ : _) = []
listToTuple (_ : _) [] = []
listToTuple (x1:x1s) (x2:x2s) =  [(x1,x2)] ++ listToTuple x1s x2s

rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable n passwords__ = Map.fromList (listToTuple (rainbowTableRecur n passwords__) passwords__)
    where
        rainbowTableRecur :: Int -> [Passwd] -> [Hash]
        rainbowTableRecur 0 passwords_ = getHashes passwords_
        rainbowTableRecur n_ passwords = rainbowTableRecur (n_-1) (map pwReduce (getHashes passwords))


findStartPw :: Map.Map Hash Passwd-> Int -> Hash -> Maybe Passwd
findStartPw table (-1) hash = Nothing
findStartPw table width_ hash 
 | Map.lookup hash table == Nothing = findStartPw table (width_-1) (pwHash (pwReduce hash))
 | otherwise = Map.lookup hash table 

findRealPw :: Maybe Passwd -> Int -> Hash -> Maybe Passwd
findRealPw Nothing width_ hash = Nothing
findRealPw (Just password) width_ hash = findRealPwRecur password width_ hash
      where  
      findRealPwRecur password_ (-1) hash_ = Nothing
      findRealPwRecur password_ width__ hash_
        | (pwHash password_) == hash_ = Just password_
        | otherwise = findRealPwRecur (pwReduce (pwHash password_)) (width__-1) hash_

findPassword :: Map.Map Hash Passwd-> Int -> Hash -> Maybe Passwd
findPassword table width_ hash 
 | findStartPw table width_ hash == Nothing = Nothing 
 | otherwise = findRealPw (findStartPw table width_ hash) width_ hash

generateTable :: IO ()
generateTable = do
  table <- buildTable rainbowTable nLetters pwLength width height
  writeTable table filename

test1 :: IO (Maybe Passwd)
test1 = do
  table <- readTable filename
  return (Map.lookup 0 table)

test2 :: Int -> IO ([Passwd], Int)
test2 n = do
  table <- readTable filename
  pws <- randomPasswords nLetters pwLength n
  let hs = map pwHash pws
  let result = Data.Maybe.mapMaybe (findPassword table width) hs
  return (result, length result)



main :: IO ()
main = do
  generateTable
  res <- test2 10000
  print res