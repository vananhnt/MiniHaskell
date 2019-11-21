module Parser (readTRS, readTRSFile) where

import Data.List
import Text.ParserCombinators.Parsec
import TRS

-- Tokenizers

integer :: Parser Int
integer = do
  spaces
  s <- many1 digit
  spaces
  return (read s :: Int)

alphaNumBar = try alphaNum <|> char '_'

lowerWord :: Parser String
lowerWord = do
  spaces
  c <- lower
  s <- many alphaNumBar
  spaces
  return (c : s)

upperWord :: Parser String
upperWord = do
  spaces
  c <- upper
  s <- many alphaNumBar
  spaces
  return (c : s)

keyword :: String -> Parser ()
keyword s = do
  spaces
  string s
  spaces
  return ()

-- Parsers

parseParen :: Parser a -> Parser a
parseParen p = do
  keyword "("
  x <- p
  keyword ")"
  return x

parseBracket :: Parser a -> Parser a
parseBracket p = do
  keyword "["
  x <- p
  keyword "]"
  return x

parseVariable :: Parser Term
parseVariable = do
  x <- upperWord
  return (Var x)

parseConstant :: Parser Term
parseConstant = do
  c <- lowerWord
  return (Con c)

makePeanoNumber 0 = Con "0"
makePeanoNumber n = App (Con "s") (makePeanoNumber (n - 1))

parsePeanoNumber :: Parser Term
parsePeanoNumber = do
  n <- integer
  return (makePeanoNumber n)

makeList []       = Con "nil"
makeList (t : ts) = App (App (Con "cons") t) (makeList ts)

parseList :: Parser Term
parseList = do
  ts <- parseBracket (sepBy parseTerm (keyword ","))
  return (makeList ts)

parseSimpleTerm :: Parser Term
parseSimpleTerm =
  try parseVariable <|>
  try parseConstant <|>
  try parsePeanoNumber <|>
  try parseList <|>
  try (parseParen parseTerm)
  

parseTerm :: Parser Term
parseTerm = do
  ts <- sepBy1 parseSimpleTerm spaces
  return (foldl1 App ts)

parseRule :: Parser Rule
parseRule = do
  l <- parseTerm
  keyword "="
  r <- parseTerm
  keyword "."
  case extraVariales l r of
    [] -> return (l, r)
    x : _ -> fail ("Unknown variable: " ++ x)

parseTRS :: Parser TRS
parseTRS = do
  trs <- many parseRule
  eof
  return trs

-- Checking the variable condition.

variables (Con _)   = []
variables (Var x)   = [x]
variables (App s t) = nub (variables s ++ variables t)

extraVariales l r = nub [ x | x <- variables r, not (elem x (variables l)) ]

-- Reader functions.

readTRSFile :: String -> IO (Either ParseError TRS)
readTRSFile path = parseFromFile parseTRS path

readTRS :: String -> Either ParseError TRS
readTRS s = parse parseTRS "(direct input)" s

