import TRS
import Parser
import PatternMatching
import System.Environment

-- Pretty printers

showRule (s, t) = show s ++ " = " ++ show t ++ " ."

showTRS rules = unlines [ showRule rule | rule <- rules ]

substitute :: Term -> Substitution -> Term
substitute (Var x) σ
  | Just t <- lookup x σ = t
substitute (Con c) σ = Con c
substitute (App s t) σ = App (substitute s σ) (substitute t σ)

m1 = Var "x"
res:: Term -> Term -> Term -> Term
res m t u = substitute m st 
  where Just st = match t u 

main =
  print (res m1 t1 u1)
  -- case readTRS "add 1 Y = Y .  add (s X) Y = s (add X Y)." of
  --   Left e    -> print e
  --   --Right trs -> putStr (showTRS trs)
  --   Right trs -> do
  --     putStr (showTerm(fst (trs!!1))++"\n")
  --     putStr (showTerm (snd (trs!!1)))

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
