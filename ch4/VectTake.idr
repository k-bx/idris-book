import Data.Vect

vectTake : (len : Fin n) -> Vect n elem -> Vect (finToNat len) elem
vectTake FZ xs = []
vectTake (FS x) (y :: ys) = y :: vectTake x ys
