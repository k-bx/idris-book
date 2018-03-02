data InfIO : Type where
  Do : IO a
       -> (a -> Inf InfIO)
       -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

total
loopPrint : String -> InfIO
loopPrint msg = do
  putStrLn msg
  loopPrint msg

-- run_bad : InfIO -> IO ()
-- run_bad (Do action cont) = do res <- action
--                               run_bad (cont res)

-- data Fuel = Dry | More Fuel

-- tank : Nat -> Fuel
-- tank Z = Dry
-- tank (S k) = More (tank k)

data Fuel = Dry | More (Lazy Fuel)

total
run : Fuel -> InfIO -> IO ()
run Dry y = putStrLn "Out of fuel"
run (More fuel) (Do c f) = do
  res <- c
  run fuel (f res)

partial
forever : Fuel
forever = More forever

-- data DelayReason' = Infinite | LazyValue

-- data Delayed' : DelayReason' -> Type -> Type where
--   Delay' : (val : ty) -> Delayed' reason ty
