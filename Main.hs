import TRS
import Parser
import PatternMatching
import System.Environment

-- Pretty printers
--type Rule = (Term, Term)
--type TRS = [Rule]

showRule (s, t) = show s ++ " = " ++ show t ++ " ."

showTRS rules = unlines [ showRule rule | rule <- rules ]

--MarkedTerm Here
substitute :: Term -> Substitution -> Term
substitute (Var x) σ
  | Just t <- lookup x σ = t
substitute (Con c) σ = Con c -- MCon
substitute (App s t) σ = App (substitute s σ) (substitute t σ) --MApp

rewriteAtRoot [] t = Nothing
rewriteAtRoot ((l,r):xs) t | Just sigma <- match l t = Just (substitute r sigma)
                           | otherwise = rewriteAtRoot xs t

rewrite :: TRS -> Term -> Maybe Term
rewrite trs (Var x) = rewriteAtRoot trs (Var x) 
rewrite trs (Con c) = rewriteAtRoot trs (Con c)
rewrite trs (App l r) 
 | Just u <- rewrite trs r = Just (App l u)
 | Just u <- rewrite trs l = Just (App u r)
 | otherwise = rewriteAtRoot trs (App l r)

nf1 :: TRS -> Term -> Term
nf1 trs t 
  | Just u <- rewrite trs t = nf1 trs u 
  | otherwise = t 
----------------------------------------------
subtitute2 :: Term -> Substitution -> MTerm
subtitute2 (Var x) sigma 
  | Just t <- lookup x sigma = NF t
subtitute2 (Con c) sigma = MCon c
subtitute2 (App l r) sigma = MApp (subtitute2 l sigma) (subtitute2 r sigma) --not sure

rewriteAtRoot2 :: TRS -> Term -> MTerm
rewriteAtRoot2 ((l,r):xs) t | Just sigma <- match l t = subtitute2 r sigma
                           | otherwise = rewriteAtRoot2 xs t

-- rewrite2 :: TRS -> MTerm -> MTerm
-- rewrite2 trs t = u 

-- nf2 :: TRS -> MTerm -> MTerm
-- nf2 trs t 
--   | Just u <- rewrite2 trs t = nf2 trs u 
--   | otherwise = t 

test trs x = rewriteAtRoot2 trs x

main = do
  --print  (test x1 t1 u1)
  file : _ <- getArgs
  m <- readTRSFile file
  case m of
    Left e    -> print e
    Right trs -> do
      putStrLn (showTRS trs)
      --putStrLn (show (nf1 trs (Con "main")))
      print (test trs (Con "main"))
      -- putStrLn (show (nf2 trs (MCon "main")))
      -- putStrLn (show (nf3 trs (Hole, MCon "main")))

{-
id (f (hole 0))
      hole 
      id.hole
      f.(id.hole)
      (f.(id.hole)).0

zipper -> shift symbol from right to context
-- if function -> shift out left (context part)
   if normal form -> shift out right (term part)
   if match in subtitution -> subtitute 
-}