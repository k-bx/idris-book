import Data.Vect

total allLength : Vect len String -> Vect len Nat
allLength [] = []
allLength (word :: words) = length word :: allLength words
