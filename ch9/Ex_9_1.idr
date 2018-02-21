{- 1 -}

data MyList : a -> Type where
  Nil : MyList a
  Cons : a -> MyList a -> MyList a

data MyElem : a -> MyList a -> Type where
  MyHere : MyElem x (Cons x xs)
  MyThere : (later : MyElem x xs) -> MyElem x (Cons y xs)

{- 2 -}

data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

last123 : Last [1,2,3] 3
last123 = LastCons (LastCons LastOne)

isLast_rhs_1 : Last [] value -> Void
isLast_rhs_1 LastOne impossible
isLast_rhs_1 (LastCons _) impossible

isLast_rhs_5 : (contra : (x = value) -> Void) -> Last [x] value -> Void
isLast_rhs_5 contra LastOne = contra Refl
isLast_rhs_5 _ (LastCons LastOne) impossible
isLast_rhs_5 _ (LastCons (LastCons _)) impossible

isLast_rhs_3 : (contra : Last (y :: xs) value -> Void) ->
               Last (x :: (y :: xs)) value ->
               Void
isLast_rhs_3 contra (LastCons prf) = contra prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No isLast_rhs_1
isLast (x :: []) value = case decEq x value of
                              (Yes Refl) => Yes LastOne
                              (No contra) => No (isLast_rhs_5 contra)
isLast (x :: (y :: xs)) value = case isLast (y::xs) value of
                                     (Yes prf) => Yes (LastCons prf)
                                     (No contra) => No (isLast_rhs_3 contra)
