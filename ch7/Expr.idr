data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

eval : (Neg num, Integral num, Abs num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub

Abs ty => Abs (Expr ty) where
    abs = Abs

(Eq ty, Abs ty, Integral ty, Neg ty) => Eq (Expr ty) where
    (==) n m = eval n == eval m

Show ty => Show (Expr ty) where
    show (Val n1) = "Val (" ++ show n1 ++ ")"
    show (Add n1 n2) = "Add (" ++ show n1 ++ ") (" ++ show n2 ++ ")"
    show (Sub n1 n2) = "Add (" ++ show n1 ++ ") (" ++ show n2 ++ ")"
    show (Mul n1 n2) = "Add (" ++ show n1 ++ ") (" ++ show n2 ++ ")"
    show (Div n1 n2) = "Add (" ++ show n1 ++ ") (" ++ show n2 ++ ")"
    show (Abs n1) = "Add (" ++ show n1 ++ ")"

(Neg num, Integral num, Abs num) => Cast (Expr num) num where
    cast expr = eval expr
