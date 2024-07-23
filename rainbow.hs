import RainbowAssign
import qualified Data.Map as Map
import Data.Maybe
import Data.List (nub)

pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 8            -- length of each password
nLetters = 5            -- number of letters to use in passwords: 5 -> a-e
width = 40              -- length of each chain in the table
height = 1000           -- number of "rows" in the table
filename = "table.txt"  -- filename to store the table


pwReduce :: Hash -> Passwd
-- convert to int before calling helper, then map and reverse to get reduced password
pwReduce hash = reverse (take pwLength (map toLetter (convert (fromEnum hash))))

convert :: Int -> [Int]
-- create list by adding remainder to result of recursively calling convert
convert hash = mod hash nLetters : convert (div hash nLetters)


rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
-- use built-in zip function to combine hash with initial passwords then use map to map hash to passwords
rainbowTable n passwords = Map.fromList (zip (rainbowHelp n passwords) passwords)

rainbowHelp :: Int -> [Passwd] -> [Hash]
-- return list of hash values when n = 0
rainbowHelp 0 passwords = map pwHash passwords
-- apply pwHash to each password, then reduce using pwReduce, map them and recursively call function with n-1
rainbowHelp n passwords = rainbowHelp (n - 1) (map (pwReduce . pwHash) passwords)


findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword table width hash = do
  -- call passBegin to get the initial password
  begin <- passBegin table width hash
  -- use that begin to call helper function to look for password, return nothing if not found
  pwHelp begin width hash
  
passBegin :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
-- if width reached -1 then no password found
passBegin table (-1) hash = Nothing
passBegin table width hash
  -- if hash not present in table, then recursively call function with width-1
  | isNothing (Map.lookup hash table) = passBegin table (width - 1) (pwHash (pwReduce hash))
  -- but if hash found then return the password
  | otherwise = Map.lookup hash table

pwHelp :: Passwd -> Int -> Hash -> Maybe Passwd
pwHelp password width hash
  -- if hash of password is equal to hash then just return password
  | pwHash password == hash = Just password
  -- if width reaches 0 then we could not find the password, so return nothing
  | width == 0 = Nothing
  -- else recursively call the function with width-1 to look again
  | otherwise = pwHelp (pwReduce (pwHash password)) (width - 1) hash
  

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
