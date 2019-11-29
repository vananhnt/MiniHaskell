-- name: NGUYEN THI VAN ANH
-- id: 1910407
-- acknowledgements: 
--------------------------------------
import TRS
import Parser
import PatternMatching
import System.Environment
import Data.Maybe

-- Pretty printers
--type Rule = (Term, Term)
--type TRS = [Rule]

showRule (s, t) = show s ++ " = " ++ show t ++ " ."

showTRS rules = unlines [ showRule rule | rule <- rules ]

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
toTerm :: MTerm -> Term
toTerm (NF f) = f
toTerm (MCon c) = Con c
toTerm (MApp l r) = App (toTerm l) (toTerm r)

subtitute2 :: Term -> Substitution -> MTerm
subtitute2 (Var x) sigma 
  | Just t <- lookup x sigma = NF t
subtitute2 (Con c) sigma = MCon c
subtitute2 (App l r) sigma  = MApp (subtitute2 l sigma) (subtitute2 r sigma) 

rewriteAtRoot2 :: TRS -> Term -> Maybe MTerm
rewriteAtRoot2 [] t = Nothing
rewriteAtRoot2 ((l,r):xs) t | Just sigma <- match l t = Just (subtitute2 r sigma)
                            | otherwise = rewriteAtRoot2 xs t

rewrite2 :: TRS -> MTerm -> MTerm
rewrite2 trs (NF f) = NF f
rewrite2 trs (MCon s) 
  | Just u <- rewriteAtRoot2 trs (Con s) = u
  | otherwise = MCon s
rewrite2 trs (MApp (MApp x y) r) = MApp (rewrite2 trs (MApp x y)) r     -- r : MApp x y
rewrite2 trs (MApp l (MApp x y)) = MApp l (rewrite2 trs (MApp x y))     -- l : MApp x y
rewrite2 trs (MApp l r)                                                 -- otherwise
      | Just u <- rewriteAtRoot2 trs (toTerm(MApp l r)) = u             -- if rewritable
      | otherwise = NF (toTerm(MApp l r))                               -- else Normal form 

nf2 :: TRS -> MTerm -> Term
nf2 trs (NF f) = f
nf2 trs t = nf2 trs (rewrite2 trs t) 
----------------------------------------------
--data Context = Hole| CApp1 Context Term | CApp2 MTerm Context
--type Zipper = (Context u, MTerm)
--data MTerm = MApp MTerm MTerm | MCon String | NF Term
tranform :: MTerm -> Term
tranform (MCon s) = Con s
tranform (NF x) = x
tranform (MApp l r) = App (tranform l) (tranform r)
-- rewrite3 R (Cu1, t1) = (Cu2, t2) if (Cu1, t1) -> (Cu2, t2)
rewrite3 :: TRS -> Zipper -> Zipper

rewrite3 trs (CApp1 c t, NF x) 
  | Just u <- rewriteAtRoot2 trs (App x t) = (c, u)
  | otherwise = (c, NF (App x t))
rewrite3 trs (CApp2 mt c, NF x) 
  | Just u <- rewriteAtRoot2 trs x = (CApp2 mt c, u)
  | otherwise = (CApp1 c x, mt)
rewrite3 trs (c, MCon x) 
  | Just u <- rewriteAtRoot2 trs (toTerm (MCon x)) = (c, u)
  | otherwise = (c, NF (toTerm (MCon x)))
rewrite3 trs (c, (MApp l r)) = (CApp2 l c, r)  

nf3 :: TRS -> Zipper -> Term
nf3 trs (Hole, (NF x)) = x
nf3 trs t = nf3 trs (rewrite3 trs t) 

main = do
  file : _ <- getArgs
  m <- readTRSFile file
  case m of
    Left e    -> print e
    Right trs -> do
      --putStrLn (showTRS trs)
      --putStrLn (show (nf1 trs (Con "main")))
      --putStrLn (show (nf2 trs (MCon "main")))
      putStrLn (show (nf3 trs (Hole, MCon "main")))

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
