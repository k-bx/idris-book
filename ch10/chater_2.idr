data SnocListNaive ty = EmptyNaive | SnocNaive (SnocListNaive ty) ty

reverseSnoc : SnocListNaive ty -> List ty
reverseSnoc EmptyNaive = []
reverseSnoc (SnocNaive xs x) = x :: reverseSnoc xs

data SnocList : List a -> Type where
  Empty : SnocList []
  Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])

snocListHelp : (snoc : SnocList input) -> (rest : List a) ->
               SnocList (input ++ rest)
snocListHelp {input = input} snoc [] = rewrite appendNilRightNeutral input
                                       in snoc
snocListHelp {input = input} snoc (x :: xs)
  = rewrite appendAssociative input [x] xs in
    snocListHelp (Snoc snoc {x}) xs

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelp Empty xs

total
myReverseHelper : (input : List a) -> SnocList input -> List a
myReverseHelper [] Empty = []
myReverseHelper (xs ++ [x]) (Snoc rec) = x :: myReverseHelper xs rec

myReverse : List a -> List a
myReverse input = myReverseHelper input (snocList input)

myReverse2 : List a -> List a
myReverse2 input with (snocList input)
  myReverse2 [] | Empty = []
  myReverse2 (xs ++ [x]) | (Snoc rec) = x :: myReverse2 xs | rec

total
isSuffix : Eq a => List a -> List a -> Bool
isSuffix input1 input2 with (snocList input1)
  isSuffix [] input2 | Empty = True
  isSuffix (xs ++ [x]) input2 | (Snoc xsrec) with (snocList input2)
    isSuffix (xs ++ [x]) [] | (Snoc xsrec) | Empty = False
    isSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xsrec) | (Snoc ysrec)
               = if x == y then isSuffix xs ys | xsrec | ysrec
                           else False
