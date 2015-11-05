import Data.Array
squares = array ((1,1), (10,10)) [ ((i, j), i*j) | i <- [1..10], j <- [1..10] ]

arrDo :: (Num a, Num b, Num d) => Array (a, b) d -> Array (a, b) d

arrSwap :: (Ix t, Ix t1) => Array (t, t1) e -> (t, t1) -> (t, t1) -> Array (t, t1) e

indexTrans :: Ix a => Array a e -> Integer -> a

indexTrans arr i = (range $ bounds arr) !! (fromIntegral i)
arrDo arr = arr

arrSwap arr (m1x,m1y) (m2x,m2y) = arr//[((m1x,m1y), arr ! (m2x, m2y)), ((m2x,m2y), arr ! (m1x, m1y))]

arrSize arr = (x2-x1+1) * (y2-y1+1)
  where ((x1,y1), (x2,y2)) = bounds arr


bSortIter :: Array (Integer, Integer) e -> [Integer]
bSortIterList :: Array (Integer, Integer) e -> [(Integer, Integer)]
bSortIterKeyList :: Array (Integer, Integer) e -> Integer -> [(Integer, Integer)]

bSortHelper
  :: (Ord a, Ix t, Ix t1) =>
       Array (t, t1) a -> [Integer] -> Array (t, t1) a

bSort arr = bSortHelper arr (bSortIter arr)

bSortIter arr = take (sint * sint - 1) (cycle [0..s-2])
  where s = arrSize arr
        sint = fromIntegral s

bSortIterList arr = bSortIterKeyList arr size
  where size = arrSize arr

bSortIterKeyList arr iter
  | iter > 0 = bSortIterKeyList arr (iter-1) ++ [indexTrans arr j | j <- [0..size - 2]]
  | otherwise = []
  where size = arrSize arr

bSortHelper arr [] = arr
bSortHelper arr (i:is)
  | (arr ! (p1_1, p1_2)) > (arr ! (p2_1, p2_2)) = bSortHelper (arrSwap arr (p1_1, p1_2) (p2_1, p2_2)) is
  | otherwise = bSortHelper arr is
    where (p1_1, p1_2) = indexTrans arr i
          (p2_1, p2_2) = indexTrans arr (i+1)

