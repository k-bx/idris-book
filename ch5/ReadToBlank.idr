import Data.Vect

readToBlank : IO (List String)
readToBlank = go Nil
  where
    go : List String -> IO (List String)
    go acc = do
      x <- getLine
      if (x == "")
        then pure acc
        else go (acc ++ [x])

readAndSave : IO ()
readAndSave = do
  contents <- readToBlank
  putStrLn "Please enter a filename where to store the contents into:"
  fileName <- getLine
  Right () <- writeFile fileName (concat (intersperse "\n" contents))
    | Left e => printLn e
  pure ()

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
    (Right fp) <- openFile filename Read
      | Left e => do printLn e; emptyRes
    r <- go fp
    closeFile fp
    pure r
  where
    emptyRes : IO (n ** Vect n String)
    emptyRes = pure (_ ** [])
    go : File -> IO (n ** Vect n String)
    go fp = do
      putStrLn "> go"
      False <- fEOF fp
        | True => do putStrLn "> got eof"; pure (_ ** [])
      Right l <- fGetLine fp
        | Left e => do printLn e; pure (_ ** [])
      (_ ** ls) <- go fp
      pure (_ ** l :: ls)
