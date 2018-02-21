import Data.Vect

-- data Vect : Nat -> Type -> Type where
--      Nil  : Vect Z a
--      (::) : a -> Vect k a -> Vect (S k) a

-- %name Vect xs, ys, zs

-- data Elem : a -> Vect k a -> Type where
--   Here : Elem x (x :: xs)
--   There : (later : Elem x xs) -> Elem x (y :: ys)

-- oneInVector : Elem 1 [1,2,3]
-- oneInVector = Here

removeElem : (value : a) -> (xs : Vect (S n) a) ->
             (prf : Elem value xs) ->
             Vect n a
removeElem x (x :: xs) Here = xs
removeElem {n = Z} value (y :: []) (There later) = absurd later
removeElem {n = (S k)} value (y :: ys) (There later) =
  y :: removeElem value ys later

removeElem_auto : (value : a) -> (xs : Vect (S n) a) ->
                  {auto prf : Elem value xs} -> Vect n a
removeElem_auto value xs {prf} = removeElem value xs prf

-- removeElem : (value : a) -> (xs : Vect (S n) a) ->
--              {auto prf : Elem value xs} ->
--              Vect n a
-- removeElem value (value :: ys) {prf = Here} = ys
-- removeElem {n = Z} value (y :: []) {prf = There later} = absurd later
-- removeElem {n = (S k)} value (y :: ys) {prf = There later}
--                                           = y :: removeElem value ys

-- removeElem : (value : a) -> (xs : Vect (S n) a) ->
--              Elem value xs ->
--              Vect n a
-- removeElem value (value :: ys) Here = ys
-- removeElem {n = Z} value (y :: []) (There later) = ?absrd_hole
-- removeElem {n = (S k)} value (y :: ys) (There later)
--                                           = y :: removeElem value ys later

-- removeElem : (value : a) -> (xs : Vect (S n) a) -> (prf : Elem value xs) ->
--              Vect n a
-- removeElem value (value :: ys) Here = ys
-- removeElem {n = Z} value (y :: ys) (There later) = ?removeElem_rhs_1
-- removeElem {n = (S k)} value (y :: ys) (There later)
--                                           = y :: removeElem value ys later

notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There _) impossible

notInTail : (notHere : (value = x) -> Void) -> (notThere : Elem value xs -> Void) ->
            Elem value (x :: xs) -> Void
notInTail notHere notThere Here = notHere Refl
notInTail notHere notThere (There later) = notThere later

myIsElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
myIsElem value [] = No notInNil
myIsElem value (x :: xs) =
  (case decEq value x of
        (Yes Refl) => Yes Here
        (No notHere) => (case myIsElem value xs of
                             (Yes prf) => Yes (There prf)
                             (No notThere) => No (notInTail notHere notThere)))
