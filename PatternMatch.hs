data Term = Var String | Fun String [Term]
  deriving (Show, Eq)

type Substitution = [(String, Term)]

match' :: Substitution -> [(Term, Term)] -> Maybe Substitution
match' sigma [] = Just sigma
match' sigma ((Fun f ts,     Fun g us) : tus)
  | f == g = match' sigma (zip ts us ++ tus)
match' sigma ((Var x, u) : tus)
  | Just u' <- lookup x sigma, u == u' =
      match' sigma tus
  | Nothing <- lookup x sigma =
      match' ((x, u) : sigma) tus
match' _ _ = Nothing

match :: Term -> Term -> Maybe Substitution
match t u = match' [] [(t,u)]

-- add(0,x)
t1 = Fun "add" [Fun "0" [], Var "x"]
-- add(0,s(x))
u1 = Fun "add" [Fun "0" [], Fun "s" [Var "x"]]
-- add(add(0,x),x)
t2 = Fun "add" [Fun "add" [Fun "0" [], Var "x"], Var "x"]
-- add(add(0,s(x)),0)
u2 = Fun "add" [Fun "add" [Fun "0" [], Fun "s" [Var "x"]],
                Fun "0" []]

main = do
  print (match t1 u1)
  print (match t2 u2)