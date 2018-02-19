import Data.Vect

twoPlusTwoNotFive : 2 + 2 = 5 -> Void
twoPlusTwoNotFive Refl impossible

-- -- Doesn't compile
-- twoPlusTwoNotFour : 2 + 2 = 4 -> Void
-- twoPlusTwoNotFour Refl impossible

valueNotSuc : (x : Nat) -> x = S x -> Void
valueNotSuc _ Refl impossible

zeroNotSuc : (0 = S k) -> Void
zeroNotSuc Refl impossible

sucNotZero : (S k = 0) -> Void
sucNotZero Refl impossible

noRec : (contra : (k = j) -> Void) -> (S k = S j) -> Void
noRec contra Refl = contra Refl

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat Z Z = Yes Refl
checkEqNat Z (S k) = No zeroNotSuc
checkEqNat (S k) Z = No sucNotZero
checkEqNat (S k) (S j) = case checkEqNat k j of
                              (Yes prf) => Yes (cong prf)
                              (No contra) => No (noRec contra)

-- actual exercises

headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
       (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
       (contra : (xs = ys) -> Void) -> ((x :: xs) = (x :: ys)) -> Void
tailUnequal contra Refl = contra Refl

-- data MyVect : (len:Nat) -> (elem:Type) -> Type where
--   MkMyVect : Vect len elem -> MyVect len elem

-- DecEq a => DecEq (MyVect n a) where
--   -- decEq : (x1 : Vect n a) -> (x2 : Vect n a) -> Dec (x1 = x2)
--   decEq (MkMyVect xs) (MkMyVect ys) = go xs ys
--     where
--       go : (x1 : Vect n a) -> (x2 : Vect n a) -> Dec (x1 = x2)
--       go = ?go_rhs

data MyVect : Nat -> Type -> Type where
  Nil : MyVect Z a
  (::) : a -> MyVect k a -> MyVect (S k) a

%name MyVect xs, ys, zs

myHeadUnequal : DecEq a => {xs : MyVect n a} -> {ys : MyVect n a} ->
       (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
myHeadUnequal contra Refl = contra Refl

myTailUnequal : DecEq a => {xs : MyVect n a} -> {ys : MyVect n a} ->
       (contra : (xs = ys) -> Void) -> ((x :: xs) = (x :: ys)) -> Void
myTailUnequal contra Refl = contra Refl

DecEq a => DecEq (MyVect n a) where
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys) =
    (case decEq x y of
          (Yes Refl) =>
            (case decEq xs ys of
                  (Yes Refl) => Yes Refl
                  (No contra) => No (myTailUnequal contra))
          (No contra) => No (myHeadUnequal contra))
