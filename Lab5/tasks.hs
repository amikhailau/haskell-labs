import Data.Char
import Data.List

transpositionsX :: [a] -> [[a]]
transpositionsX [] = []
transpositionsX xss = removeSmallOnes (transpositionsHelperX xss []) (length xss) []

transpositionsHelperX :: [a] -> [[a]] -> [[a]]
transpositionsHelperX [] xss = xss
transpositionsHelperX (y:ys) [] = transpositionsHelperX ys [[y]]
transpositionsHelperX (y:ys) xss = transpositionsHelperX ys (xss ++ (makeNewTranspositions y xss []))

makeNewTranspositions :: a -> [[a]] -> [[a]] -> [[a]]
makeNewTranspositions _ [] newTrans = newTrans
makeNewTranspositions x (trans:oldTrans) newTrans = makeNewTranspositions x oldTrans (newTrans ++ (insertAtEachPos x trans [] (length trans)))

insertAtEachPos :: a -> [a] -> [[a]] -> Int -> [[a]]
insertAtEachPos _ _ result (-1) = result
insertAtEachPos x xs result p = let (first, last) = splitAt p xs
                                in insertAtEachPos x xs (result ++ [(first ++ [x] ++ last)]) (p-1)

removeSmallOnes :: [[a]] -> Int -> [[a]] -> [[a]]
removeSmallOnes [] _ result = result
removeSmallOnes (x:xss) expected result = if length x == expected then removeSmallOnes xss expected (x:result)
                                                                  else removeSmallOnes xss expected result


data Tree = Empty
		  | Node Tree String Int Tree

treeBuild :: String -> Tree
treeBuild text = foldl addWord Empty (processText text)

addWord :: Tree -> String -> Tree
addWord Empty word = (Node Empty word 1 Empty)
addWord (Node leftson value occurrences rightson) word = if word > value then Node leftson value occurrences (addWord rightson word)
                                                         else if word < value then Node (addWord leftson word) value occurrences rightson
					                                          else Node leftson value (occurrences + 1) rightson

treeGet :: Tree -> String -> Int
treeGet (Empty) _ = 0
treeGet (Node leftson value occurrences rightson) word = if word > value then treeGet rightson word
                                                         else if word < value then treeGet leftson word
														      else occurrences

processText :: String -> [String]
processText text = words (fmap (\c -> if isLower c || isSpace c then c; else ' ') (fmap toLower text))