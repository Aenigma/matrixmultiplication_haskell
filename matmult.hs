import Data.Char
import Data.List
import System.IO
import Data.Array

-- sort 
arrSwap :: (Ix t, Ix t1) => Array (t, t1) e -> (t, t1) -> (t, t1) -> Array (t, t1) e
indexTrans :: Ix a => Array a e -> Integer -> a
indexTrans arr i = (range $ bounds arr) !! (fromIntegral i)
bSortIter :: Array (Integer, Integer) e -> [Integer]
bSortHelper :: (Ord a, Ix t, Ix t1) =>
       Array (t, t1) a -> [Integer] -> Array (t, t1) a

-- arr/list conversion
listToArr :: [[a]] -> Array (Integer, Integer) a
arrToMat :: Array(Integer, Integer) a -> Integer -> [[a]]


-- main, uses IO monad
main :: IO ()

-- matrix parsing
wordSplit :: String -> [String]
wordsToInteger :: [String] -> [Integer]
splitN :: [a] -> Int -> ([a], [a])
splitHalf :: [a] -> ([a], [a])
splitNList :: [t] -> Int -> [[t]]

-- matrix specific
matMult :: Num a => [[a]] -> [[a]] -> [[a]]
vecMatMult :: Num a => [a] -> [[a]] -> [a]
matPrettyPrint :: Show a => [[a]] -> String

matPrettyPrint mat = intercalate "\n"
  [ "[" ++ intercalate ", "
    [ show element | element <- row]
    ++ "]"
  | row <- mat ]

main = withFile "COSC450_P2_Data.txt" ReadMode (\ handle ->
     do contents <- hGetContents handle
        let l = wordsToInteger $ wordSplit contents
        let (l1, l2) = splitHalf l

        let size2 = 5
        let size1 = quot (length l1) size2

        let mat1 = splitNList l1 size1
        let mat2 = splitNList l2 size2

        let mat3 = matMult mat1 mat2

        let arr = bSort $ listToArr mat3

        let sortedList = arrToMat arr (toInteger size2)


        withFile "COSC450_P2_Output.txt" WriteMode (\hwrite ->
          do
          hPutStrLn hwrite "Matrix 1"
          hPutStrLn hwrite $ matPrettyPrint mat1
          hPutStrLn hwrite []
          hPutStrLn hwrite "Matrix 2"
          hPutStrLn hwrite $ matPrettyPrint mat2
          hPutStrLn hwrite []
          hPutStrLn hwrite "Product Matrix"
          hPutStrLn hwrite $ matPrettyPrint mat3
          hPutStrLn hwrite []
          hPutStrLn hwrite "Sorted Matrix"
          hPutStrLn hwrite $ matPrettyPrint sortedList)

        putStrLn "Matrix 1"
        putStrLn $ matPrettyPrint mat1
        putStrLn []
        putStrLn "Matrix 2"
        putStrLn $ matPrettyPrint mat2
        putStrLn []
        putStrLn "Product Matrix"
        putStrLn $ matPrettyPrint mat3
        putStrLn []
        putStrLn "Sorted Matrix"
        putStrLn $ matPrettyPrint sortedList)

wordsToInteger = map (\a -> read a :: Integer)

matMult (a:as) bs = vecMatMult a (transpose bs) : matMult as bs
matMult [] _ = []

vecMatMult a (b:bs) = sum (zipWith (*) a b) : vecMatMult a bs
vecMatMult _ [] = []

splitHalf l = splitN l 2
splitN l n = splitAt (quot (length l) n) l


arrSwap arr (m1x,m1y) (m2x,m2y) = arr//[((m1x,m1y), arr ! (m2x, m2y)), ((m2x,m2y), arr ! (m1x, m1y))]

arrSize arr = (x2-x1+1) * (y2-y1+1)
  where ((x1,y1), (x2,y2)) = bounds arr

bSort arr = bSortHelper arr (bSortIter arr)

bSortIter arr = take (sint * sint - 1) (cycle [0..s-2])
  where s = arrSize arr
        sint = fromIntegral s

bSortHelper arr [] = arr
bSortHelper arr (i:is)
  | (arr ! (p1_1, p1_2)) > (arr ! (p2_1, p2_2)) = bSortHelper (arrSwap arr (p1_1, p1_2) (p2_1, p2_2)) is
  | otherwise = bSortHelper arr is
    where (p1_1, p1_2) = indexTrans arr i
          (p2_1, p2_2) = indexTrans arr (i+1)

splitNList [] _ = []
-- doesn't work if length l is not divisible by n...
splitNList l n = lres : splitNList ls n
  where (lres, ls) = splitAt n l

wordSplit s = case dropWhile isSpace s of
  "" -> []
  s' -> w : wordSplit s''
          where (w, s'') = break isSpace s'

listToArr li = arr [((toInteger i, toInteger j), li !! (i-1) !! (j-1)) | i <- [1..d1], j <- [1..d2]]
  where d1 = length li
        d2 = length $ head li
        arr = array ((1,1), (toInteger $ d1, toInteger $ d2)) 

arrToMat arr split = splitNList [ arr ! i | i <- ilist ] (fromIntegral split)
  where ilist = range $ bounds arr

