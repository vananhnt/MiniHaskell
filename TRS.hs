module TRS where

import Data.List

data Term = Var String | Con String | App Term Term
  deriving Eq

data MTerm = MApp MTerm MTerm | MCon String | NF Term
  deriving Show

data Context = Hole| CApp1 Context Term | CApp2 MTerm Context

type Substitution = [(String, Term)]
             
type Zipper = (Context, MTerm)

-- s. s 0
--x1 = MApp (MCon "s") (NF (App (Con "s") (Con "0")))
type Rule = (Term, Term)

type TRS = [Rule]

-- Pretty printers for terms

number (Con "0")         = Just 0
number (App (Con "s") t)
  | Just n <- number t   = Just (n + 1)
number _                 = Nothing

list (Con "nil")  = Just []
list (App (App (Con "cons") s) t)
  | Just ss <- list t = Just (s : ss)
list _ = Nothing

showTerm t
  | Just n <- number t = show n
showTerm t
  | Just us <- list t = "[" ++ intercalate "," [ showTerm u | u <- us ] ++ "]"
showTerm (App s t) = showTerm s ++ " " ++ showSimpleTerm t
showTerm t         = showSimpleTerm t
showSimpleTerm t
  | Just n <- number t = show n
showSimpleTerm t
  | Just us <- list t  = "[" ++ intercalate "," [ showTerm u | u <- us ] ++ "]"
showSimpleTerm (Var x) = x
showSimpleTerm (Con f) = f
showSimpleTerm t       = "(" ++ showTerm t ++ ")"

instance Show Term where
  show t = showTerm t
