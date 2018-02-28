import Data.Primitives.Views

{- 1 -}

total
every_other : Stream a -> Stream a
every_other (x :: (y :: xs)) = y :: every_other xs

{- 2 -}

data InfList : Type -> Type where
  (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

%name InfList xs, ys, zs

total
getPrefix : Nat -> InfList a -> List a
getPrefix Z x = []
getPrefix (S k) (x :: xs) = x :: getPrefix k xs

total
countFrom : Integer -> InfList Integer
countFrom x = x :: countFrom (x + 1)

Functor InfList where
  -- map : (func : a -> b) -> InfList a -> InfList b
  map f (value :: xs) = f value :: map f xs

{- 3 -}

data Face : Type where
  Heads : Face
  Tails : Face

total
getFace : (x : Int) -> Face
getFace x with (divides x 2)
  getFace ((2 * div) + rem) | (DivBy prf) =
    if rem == 0 then Heads else Tails

total
coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips Z xs = []
coinFlips (S k) (x :: xs) = getFace x :: coinFlips k xs

{- 4 -}

square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx
  = approx :: square_root_approx number ((approx + (number / approx)) / 2)

square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) ->
                    (approxs : Stream Double) -> Double
square_root_bound Z number bound (value :: xs) = value
square_root_bound (S k) number bound (value :: xs)
  = if (abs(value * value - number) < bound)
    then value
    else square_root_bound k number bound xs

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000000001
                                       (square_root_approx number number)
