import Control.Exception
import Control.Monad
import System.IO
import Data.Map as M
import Data.List as L
import Data.Ord as O

count_words :: [String] -> [(String, Int)]
count_words words = L.sortBy (O.comparing $ (negate . snd)) $ M.toAscList $ count_words_helper words (M.fromList [])

count_words_helper :: [String] -> M.Map String Int -> M.Map String Int
count_words_helper [] map = map
count_words_helper (w:words) map = count_words_helper words (insertWith (+) w 1 map)

main = do
  putStrLn "File name"
  fileName <- getLine
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  print $ show $ count_words $ words contents
    
