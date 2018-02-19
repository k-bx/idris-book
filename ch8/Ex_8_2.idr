import Data.Vect

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = rewrite plusZeroRightNeutral m in Refl
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in
                         rewrite plusSuccRightSucc m k in Refl

myPlusCommutesBook : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutesBook Z m = rewrite plusZeroRightNeutral m in Refl
myPlusCommutesBook (S k) m = rewrite myPlusCommutesBook k m in
                         rewrite plusSuccRightSucc m k in Refl

--

krmyReverse : Vect n a -> Vect n a
krmyReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n+m) a
        reverse' {n} acc [] = rewrite (plusZeroRightNeutral n) in acc
        reverse' acc (x :: xs) = ?rhs (reverse' (x :: acc) xs)
        -- reverse' {n} {m} acc (x :: xs)
        --   = let rev = reverse' (x :: acc) xs in
        --     rewrite sym (plusSuccRightSucc n m) in rev

reverseProof_nil : Vect n a -> Vect (plus n 0) a
reverseProof_nil {n} xs = rewrite plusZeroRightNeutral n in xs

reverseProof_xs : Vect (S n + k) a -> Vect (plus n (S k)) a
-- reverseProof_xs {n} {k} xs = rewrite plusSuccRightSucc n k in xs
reverseProof_xs {n} {k} xs = rewrite sym (plusSuccRightSucc n k) in xs

-- plusSuccRightSucc n k => S (n + k) = n + S k
-- sym (plusSuccRightSucc n k) => n + S k = S (n + k)

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n+m) a
        reverse' acc [] = reverseProof_nil acc
        reverse' acc (x :: xs)
                        = reverseProof_xs (reverse' (x::acc) xs)
