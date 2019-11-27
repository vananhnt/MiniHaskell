module PatternMatching where
import TRS
import Parser

-- data Term = Var String | Con String | App Term Term
--     deriving (Show, Eq)
-- K x y = App ( App K x) y

-- x -> Term

t1 = App (App (Con "add") (Con "0")) (Var "x")
u1 = App (App (Con "add") (Con "0")) (App (Con "s") (Var "x"))
x1 = App (Con "s") (Var "x")
-- drop marked part to patern matching 

--sigma is the list of saved subtitutions
match' :: Substitution -> [(Term, Term)] -> Maybe Substitution
match' sigma [] = Just sigma
match' sigma ((App t1 t2, App u1 u2) :tus) 
    = match' sigma ((t1, u1):(t2, u2):tus)
match' sigma ((Con f, Con g) :tus)
    | f == g = match' sigma tus
match' sigma ((Var x, u) : tus)
    | Just u' <- lookup x sigma, u == u' = match' sigma tus
    | Nothing <- lookup x sigma = match' ((x, u) : sigma) tus
match' _ _ = Nothing

match :: Term -> Term -> Maybe Substitution
match t u = match' [] [(t,u)]
