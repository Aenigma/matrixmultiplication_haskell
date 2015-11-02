import Data.Char
import Data.List
import System.IO

main :: IO ()

wordSplit :: String -> [String]
wordsToInteger :: [String] -> [Integer]

matMult :: Num a => [[a]] -> [[a]] -> [[a]]
vecMatMult :: Num a => [a] -> [[a]] -> [a]

matPrettyPrint :: Show a => [[a]] -> String

splitN :: [a] -> Int -> ([a], [a])
splitHalf :: [a] -> ([a], [a])
splitNList :: [t] -> Int -> [[t]]

matPrettyPrint mat = intercalate "\n"
  [ "[" ++ intercalate ", "
    [ show element | element <- row]
    ++ "]"
  | row <- mat ]

main = withFile "COSC450_P1_Data.txt" ReadMode (\ handle ->
     do contents <- hGetContents handle
        let l = wordsToInteger $ wordSplit contents
        let (l1, l2) = splitHalf l

        let size2 = 5
        let size1 = quot (length l1) size2

        let mat1 = splitNList l1 size1
        let mat2 = splitNList l2 size2

        let mat3 = matMult mat1 mat2

        putStrLn "Matrix 1"
        putStrLn $ matPrettyPrint mat1
        putStrLn []
        putStrLn "Matrix 2"
        putStrLn $ matPrettyPrint mat2
        putStrLn []
        putStrLn "Product Matrix"
        putStrLn $ matPrettyPrint mat3
        putStrLn [])

wordsToInteger = map (\a -> read a :: Integer)

matMult (a:as) bs = vecMatMult a (transpose bs) : matMult as bs
matMult [] _ = []

vecMatMult a (b:bs) = sum (zipWith (*) a b) : vecMatMult a bs
vecMatMult _ [] = []

splitHalf l = splitN l 2
splitN l n = splitAt (quot (length l) n) l

splitNList [] _ = []

-- doesn't work if length l is not divisible by n...
splitNList l n = lres : splitNList ls n
  where (lres, ls) = splitAt n l

wordSplit s = case dropWhile isSpace s of
  "" -> []
  s' -> w : wordSplit s''
          where (w, s'') = break isSpace s'
