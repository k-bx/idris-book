import Data.List

-- New kind of object I made up
data SomeObject = SomeObA | SomeObB | SomeObC | SomeObD

-- All possible objects in our system
data Object = Num Integer
            | SomeObj SomeObject

-- Set is a list of objects and a list of subsets
data Set = MkSet (List Object) (List Set)
         | MkAllSubsets Set -- 2ᴬ

-- Empty set
Ø : Set
Ø = MkSet [] []

-- Some operations like "elem of" should work for both objects and
-- sets
data ObjOrSet = MkOrObj Object
              | MkOrSet Set

-- Possible statements
data Statement = ElemOf ObjOrSet Set -- ∈
               | NotElemOf ObjOrSet Set -- ∉
               | IsSubset Set Set -- ⊂
               | IsSubsetBk Set Set -- ⊃

-- TODO: implement when you'll have an Eq instance
-- isProperSubset : Set -> Set -> Bool
-- isProperSubset Ø _ = False
-- isProperSubset s1 s2 = not (s1 == s2)

-- TODO: 2ᴬ => B ∈ 2ᴬ iff B ⊂ A
subsetsRule : Statement -> Set -> Maybe Statement
--subsetsRule (IsSubset b a) (MkAllSubsets a') = Just (ElemOf (MkOrSet b) s)

-- Exercises from loc 165:
-- Determine whether each of the following statements is true or false:
-- a) For each set A, A ∈ 2ᴬ
ex_165_a : Set -> Statement -- ElemOf (MkOrSet A) (SubsetsOf A)
ex_165_a = ?ex_165_a_rhs
-- ex_165_a a = ElemOf (MkOrSet a) (SubsetsOf a)

-- symbols: ∈∉⊂⊃ᴬ∃∀
