import Data.Vect

insert : Ord elem => (x : elem) -> (xsSorted : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = if x < y then x :: y :: xs
                              else y :: (insert x xs)

total insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: []) = [x]
insSort (x :: xs) = let xsSorted = insSort xs in
                    insert x xsSorted

total insSortKr : Ord elem => Vect n elem -> Vect n elem
insSortKr [] = []
insSortKr (x :: []) = [x]
insSortKr (x :: xs) = insertKr x (insSortKr xs)
  where
    -- insertKr : elem -> Vect n elem -> Vect (S n) elem
    insertKr el [] = [el]
    insertKr el (y :: ys) =
      case el > y of
        False => el :: y :: ys
        True => y :: (insertKr el ys)

    -- insert el [] = [el]
    -- insert el (y :: ys) = ?ins
      -- if el < y
      --   then el :: y :: ys
      --   else y :: (insert el ys)
