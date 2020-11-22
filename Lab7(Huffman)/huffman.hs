import Data.List as L
import Data.Map as M


-- HuffmanTree declaration and helper functions
data HuffmanTree v = Node Int (HuffmanTree v) (HuffmanTree v)
                   | Leaf v Int

getFrequency :: HuffmanTree a -> Int
getFrequency (Node f _ _) = f
getFrequency (Leaf _ f) = f

compareTrees :: HuffmanTree a -> HuffmanTree a -> Ordering
compareTrees t1 t2 = if getFrequency t1 > getFrequency t2 then GT
                     else if getFrequency t1 < getFrequency t2 then LT
					 else EQ
--


-- Functions to process text into HuffmanTree
countSymbolsFrequency :: Ord a => [a] -> [(a, Int)]
countSymbolsFrequency list = M.toList ( M.fromListWith (+) [(c, 1) | c <- list] )

toLeaf :: (a, Int) -> HuffmanTree a
toLeaf (c, p) = Leaf c p

toLeafs :: Ord a => [a] -> [HuffmanTree a]
toLeafs s = fmap toLeaf (countSymbolsFrequency s)

sortHuffmanTrees :: Ord a => [HuffmanTree a] -> [HuffmanTree a]
sortHuffmanTrees xs = sortBy compareTrees xs
            
toHuffmanTree :: Ord a => [a] -> HuffmanTree a
toHuffmanTree str = toHuffmanTreeHelper $ sortHuffmanTrees $ toLeafs str

toHuffmanTreeHelper :: Ord a => [HuffmanTree a] -> HuffmanTree a
toHuffmanTreeHelper (t:[]) = t
toHuffmanTreeHelper (t1:t2:xs) = case compareTrees t1 t2 of
    GT -> toHuffmanTreeHelper $ sortHuffmanTrees $ (Node (getFrequency t1 + getFrequency t2) t2 t1):xs
    otherwise -> toHuffmanTreeHelper $ sortHuffmanTrees $ (Node (getFrequency t1 + getFrequency t2) t1 t2):xs
--


-- Build coding table from HuffmanTree
buildCodingTable ::  Ord a => HuffmanTree a -> M.Map a String
buildCodingTable t = M.fromList $ buildCodingTableHelper t ""

buildCodingTableHelper :: HuffmanTree a -> String -> [(a, String)]
buildCodingTableHelper (Leaf v _) code = [(v, code)]
buildCodingTableHelper (Node _ t1 t2) code = (buildCodingTableHelper t1 (code ++ "0")) ++ (buildCodingTableHelper t2 (code ++ "1"))
--


-- Encode using coding table
encode :: [Char] -> M.Map Char String -> [Char]
encode text ct = encodeHelper text ct ""

encodeHelper :: [Char] -> M.Map Char String -> [Char] -> [Char]
encodeHelper [] _ newtext = newtext
encodeHelper (c:text) ct newtext = encodeHelper text ct (newtext ++ (ct ! c) ++ " ")
--


-- Decode using coding table
decode :: [Char] -> M.Map String Char -> [Char]
decode text ct = decodeHelper (words text) ct ""

decodeHelper :: [String] -> M.Map String Char -> [Char] -> [Char]
decodeHelper [] _ newtext = newtext
decodeHelper (s:text) ct newtext = decodeHelper text ct (newtext ++ ((ct ! s):[]))
--


-- Read coding table from file
readCodingTable :: String -> M.Map String Char
readCodingTable text = M.fromList $ readCodingTableHelper (words text) []

readCodingTableHelper :: [String] -> [(String, Char)] -> [(String, Char)]
readCodingTableHelper [] res = res
readCodingTableHelper (s1:s2:text) res = let c = head s1
                                         in readCodingTableHelper text (res ++ [(s2, c)])
--


-- Print coding table to file
printCodingTable :: String -> M.Map Char String -> IO()
printCodingTable filename ct = writeFile filename $ codingTableToString (M.toList ct) ""

codingTableToString :: [(Char,String)] -> String -> String
codingTableToString [] res = res
codingTableToString ((c, code):ct) res = codingTableToString ct (res ++ [c] ++ " " ++ code ++ "\n")
--


-- Run encoding
runHuffmanEncode :: IO(IO(),IO())
runHuffmanEncode = do
	contents <- readFile "in.txt"
	let codingTable = buildCodingTable $ toHuffmanTree contents
	let writeCodingTable = printCodingTable "codingTable.txt" codingTable
	let writeEncodedText = writeFile "encoded.txt" (encode contents codingTable)
	return (writeCodingTable, writeEncodedText)
	
-- Run decoding
runHuffmanDecode :: IO(IO())
runHuffmanDecode = do
	encodedContents <- readFile "encoded.txt"
	codingTableText <- readFile "codingTable.txt"
	let writeDecodedText = writeFile "decoded.txt" (decode encodedContents (readCodingTable codingTableText))
	return writeDecodedText
	
	
-- Main
main = do
	contents <- readFile "in.txt"
	let codingTable = buildCodingTable $ toHuffmanTree contents
	printCodingTable "codingTable.txt" codingTable
	writeFile "encoded.txt" (encode contents codingTable)
	
	encodedContents <- readFile "encoded.txt"
	codingTableText <- readFile "codingTable.txt"
	writeFile "decoded.txt" (decode encodedContents (readCodingTable codingTableText))