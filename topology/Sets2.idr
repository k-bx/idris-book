data Expr = Object String
          | Set String
          | AllSubsets Expr
          | ElemOf Expr Expr
          | NotElemOf Expr Expr
          | Subset Expr Expr
          | SubsetBk Expr Expr

Ø : Expr
Ø = Set "Ø"

subsetsRule : Expr -> Expr -> Maybe Expr
subsetsRule (Subset bb@(Set b) (Set a)) aa@(AllSubsets (Set a)) = Just (ElemOf bb aa)
subsetsRule _ _ = Nothing

ex_165_a : Expr -> Maybe Expr
ex_165_a (Set "A") = Just (ElemOf (Set "A") (AllSubsets (Set "A")))
ex_165_a _ = Nothing
