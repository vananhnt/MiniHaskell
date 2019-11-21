import TRS
import Parser
import System.Environment

-- Pretty printers

showRule (s, t) = show s ++ " = " ++ show t ++ " ."

showTRS rules = unlines [ showRule rule | rule <- rules ]

{-
type Substitution = [(String,Term)]

substitute :: Term -> Substitution -> Term
substitute (Var x) σ
  | Just t <- lookup x σ = t
substitute (Con c) σ =..
substitute (App s t) σ =..
-}

main =
  case readTRS "add 1 Y = Y .  add (s X) Y = s (add X Y)." of
    Left e    -> print e
    Right trs -> putStr (showTRS trs)

{-
main = do
  file : _ <- getArgs
  m <- readTRSFile file
  case m of
    Left e    -> print e
    Right trs -> do
      putStrLn (showTRS trs)
      -- putStrLn (show (nf1 trs (Con "main")))
      -- putStrLn (show (nf2 trs (MCon "main")))
      -- putStrLn (show (nf3 trs (Hole, MCon "main")))
-}
