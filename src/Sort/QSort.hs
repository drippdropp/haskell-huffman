module Sort.QSort where

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort ys ++ [x] ++ qsort zs
  where ys = [ y | y<-xs, y <= x ]
        zs = [ y | y<-xs, y >  x ]
