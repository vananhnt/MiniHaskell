import TRS
import Parser
import PatternMatching
import System.Environment

-- Pretty printers
--type Rule = (Term, Term)
--type TRS = [Rule]

showRule (s, t) = show s ++ " = " ++ show t ++ " ."

showTRS rules = unlines [ showRule rule | rule <- rules ]

substitute :: Term -> Substitution -> Term
substitute (Var x) σ
  | Just t <- lookup x σ = t
substitute (Con c) σ = Con c
substitute (App s t) σ = App (substitute s σ) (substitute t σ)

rewriteAtRoot :: TRS -> Term -> Maybe Term
rewriteAtRoot ((l, r):rs) t 
  | Just sigma <- match l t = (substitute r sigma)
  | otherwise = Nothing

rewrite :: TRS -> Term -> Maybe Term
rewrite trs (Var x) = rewriteAtRoot trs (Var x) 
rewrite trs (Con c) = rewriteAtRoot trs (Con c)
rewrite trs (App l r) 
  | Just u <- rewrite trs r = Just u
  | otherwise = nothing

m1 = Var "x"
res:: Term -> Term -> Term -> Term
res m t u = substitute m st 
  where Just st = match t u 

main =
  case readTRS "add 1 Y = Y .  add (s X) Y = s (add X Y)." of
    Left e    -> print e
    Right trs -> putStr (showTRS trs)
    Right trs -> do
      putStr (showTerm(fst (trs!!1))++"\n")
      putStr (showTerm (snd (trs!!1)))

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
