data InfIO : Type where
  Do : IO a
       -> (a -> Inf InfIO)
       -> InfIO

loopPrint : String -> InfIO
loopPrint msg = Do (putStrLn msg) (\_ => loopPrint msg)
