import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTransposed = transposeMat xs in
                             zipWith (::) x xsTransposed

add_x_y : Num a => (x : Vect m a) -> (y : Vect m a) -> Vect m a
add_x_y [] [] = []
add_x_y (x :: xs) (y :: ys) = x + y :: add_x_y xs ys

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) ->
            Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = add_x_y x y :: addMatrix xs ys
