{- This is an implementation of a Dictionary tree ADT and the 'doublets' game
 - Games run like this: You have a dictionary (the Dict ADT which is a TREE which stores char nodes
 - for the current alphabet: 0..26 meaning 'a'..='z'')
 - and you must give a start word and an end word, of the same number of letters.
 - Find a 'chain' from the start word to end node
 - A chain is valid if from one member to the next member only one letter is changed
 
  Using the search function with two strings and a chain length 
    will find you a chain from start to end using the words from word.txt (if there is one!)
    Check it out.
    

  Example: search "WHEAT" "BREAD" 8
    it will form a chain of 8 words (8 including WHEAT and BREAD)

-}

import Data.List
import Data.Char

filePath = "words.txt"

alphabetSize = 26

type Chain = [String]
data Dict = Leaf | Node Bool [Dict]

instance Show Dict where
    show (Leaf) = "L"
    show (Node b ts) = (show b) ++ "[ " ++ concatMap show ts ++ "]"

alphaIndex :: Char -> Int
alphaIndex c = ord c - ord 'A'

-- Constructs infinite list of leaves
leaves = Leaf : leaves

emptyChildren :: Int -> [Dict]
emptyChildren alphabet = take alphabet leaves

insertDict :: String -> Dict -> Dict
insertDict [c] (Node isWord ts)
    = Node isWord children
    where
        newDict  = Node True (emptyChildren alphabetSize)
        index    = alphaIndex c
        children = replaceDicts newDict ts index 
insertDict [c] Leaf
  = Node False children 
    where 
        infants  = emptyChildren alphabetSize
        children = replaceDicts (Node True (emptyChildren alphabetSize)) infants (alphaIndex c)
insertDict (c : cs) Leaf 
    = Node False children  
    where
        infants  = emptyChildren alphabetSize
        newDict  = insertDict cs Leaf
        children = replaceDicts newDict infants (alphaIndex c)
insertDict (c : cs) (Node isWord ts)
    = Node isWord children
    where
        index    = alphaIndex c
        newDict  = insertDict cs (ts !! index)
        children = replaceDicts newDict ts index 

-- Replaces at given index a dict in a list of dicts
replaceDicts :: Dict -> [Dict] -> Int -> [Dict]
replaceDicts new (d : ds) 0
  = new : ds
replaceDicts new (d : ds) index
  = d : (replaceDicts new ds (index - 1)) 

-- For when checking if a dict contains a word
nodeContains :: Dict -> Bool
nodeContains Leaf       = False
nodeContains (Node b _) = b

contains :: String -> Dict -> Bool
contains [] _ -- just for good measure
  = False
contains [c] (Node _ ts) 
  = nodeContains (ts !! alphaIndex c)
contains [c] _ 
  = False
contains xs Leaf 
  = False
contains (c : cs) (Node _ ts) 
  = contains cs (ts !! (alphaIndex c)) 

validStep :: String -> String -> Bool
validStep str1 str2 
  | length str1 /= length str2
     =  False
  | otherwise
     = countOcc False ( (zipWith (==) str1 str2) ) == 1 
      where
          countOcc c xs = length $ filter (==c) xs

validChain :: [String] -> Bool
validChain [] 
  = False
validChain (_ : []) 
  = False
validChain xs 
  = (foldl1 foldStep xs) /= [] && (length (nub xs) == length xs)
  where
      foldStep s1 s2 = if (validStep s1 s2) then s2 else []

-- Pre: maxNo must at least be two
-- Finds a chain from start to end, with maximum # of elements
-- And returns a touple of True and chain and False and [] if not found
findChain :: Dict -> String -> String -> Int -> (Bool, [String])
findChain dict start end maxNo 
  | length start /= length end
     = (False, [])
  | otherwise
     = findChain' dict start end [start] maxNo 

findChain' :: Dict -> String -> String -> [String] -> Int -> (Bool, [String])
findChain' dict curr end words maxNo 
  | curr == end 
    = if (length words <= maxNo) then (True, words) else (False, [])
  | length words > maxNo
    = (False, [])
  | otherwise    
    = oneGoodChain $ map (\w -> tryChain dict w end words maxNo)  newWords
    where
    newWords     = map (\(ind, ch) -> replace curr ch ind) combinations
    combinations = [(ind, ch) | ind <- [0..length curr - 1], ch <- ['A'..'Z']]

tryChain :: Dict -> String -> String -> Chain -> Int -> (Bool, Chain)
tryChain dict newWord end words maxNo 
  | newWord == end
    = if (length words < maxNo) then (True, (words ++ [end])) else (False, [])
  | contains newWord dict && validChain (words ++ [newWord]) 
    = findChain' dict newWord end (words ++ [newWord]) maxNo
  | otherwise
    = (False, [])

oneGoodChain :: [(Bool, Chain)] -> (Bool, Chain)
oneGoodChain []                   = (False, [])
oneGoodChain ((True, chain) : cs) = (True, chain)
oneGoodChain ((False, _) : cs)    = oneGoodChain cs

-- Pre: index must be valid
-- replaces the char at index ind with given char
replace :: String -> Char -> Int -> String
replace (ch : cs) c 0   = c : cs 
replace (ch : cs) c ind = ch : (replace cs c (ind - 1))

insertAll :: [String] -> Dict
insertAll ws = foldl (flip insertDict) root ws
    where
        root = Node False (emptyChildren alphabetSize)

upperStr :: String -> String 
upperStr = map toUpper

search str1 str2 steps = do 
        content <- readFile filePath
        mapM_ print (take 100 (words content))
        let dict = insertAll (words content)
        print $ findChain dict (upperStr str1) (upperStr str2) steps 
