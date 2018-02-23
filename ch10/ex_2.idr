import Data.List.Views
import Data.Nat.Views
import Data.Vect
import Data.Vect.Views

total
equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys with (snocList xs)
  equalSuffix [] ys | Empty = []
  equalSuffix (zs ++ [x]) ys | (Snoc xsrec) with (snocList ys)
    equalSuffix (zs ++ [x]) [] | (Snoc xsrec) | Empty = []
    equalSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xsrec) | (Snoc ysrec)
      = if x == y
           then (equalSuffix xs ys | xsrec | ysrec) ++ [x]
           else []

total
mergeSort : Ord a => Vect n a -> Vect n a
mergeSort xs with (splitRec xs)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (xs ++ ys) | (SplitRecPair lrec rrec)
    = merge (mergeSort xs | lrec) (mergeSort ys | rrec)

-- toBinary 42 == "101010"
-- 

total
toBinary : Nat -> String
toBinary k with (halfRec k)
  toBinary Z | HalfRecZ = ""
  toBinary (n + n) | (HalfRecEven rec) = (toBinary n | rec) ++ "0"
  toBinary (S (n + n)) | (HalfRecOdd rec) = (toBinary n | rec) ++ "1"
