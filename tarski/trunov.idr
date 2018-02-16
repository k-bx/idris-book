postulate N : Type
postulate lt : N -> N -> Type
postulate gt : N -> N -> Type
postulate add : N -> N -> N

postulate axiom1 : (x, y : N) -> Either (x = y) $ Either (x `lt` y) (x `gt` y)
postulate axiom2 : x `lt` y -> Not (y `lt` x)
postulate axiom3 : x `gt` y -> Not (y `gt` x)
postulate axiom4 : x `lt` y -> y `lt` z -> x `lt` z
postulate axiom5 : x `gt` y -> y `gt` z -> x `gt` z

theorem1 : Not (x `lt` x)
theorem1 contra = axiom2 contra contra

-- TODO: why this doesn't typecheck?
-- theorem1 : Not (x `lt` y)
-- theorem1 contra = axiom2 contra contra
