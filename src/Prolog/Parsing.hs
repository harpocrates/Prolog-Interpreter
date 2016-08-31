module Prolog.Parsing
  ( functor, clause, query, program
  ) where

import Prolog.Base hiding (query, unify, substitute, variables)

import Text.ParserCombinators.Parsec hiding (spaces)

-- | parse a variable: `X`
variable :: Parser (Int -> Term)
variable = do
  c <- upper
  cs <- many $ alphaNum <|> oneOf "'-_"
  return $ \n -> Var (c:cs) n


-- parse a natural number: `12`
-- syntactic sugar for n = s^n(zero)
number :: Parser (Int -> Term)
number = do
      ds <- many1 digit
      return $ \_ -> expandInteger $ read ds
  where
    expandInteger :: Integer -> Term
    expandInteger 0 = Fun "zero" []
    expandInteger n = Fun "s" [ expandInteger $ n - 1 ]


-- parse a list: `[1,2|X]`
-- syntactic sugar for `cons(1,cons(2,X))`
list :: Parser (Int -> Term)
list = between (char '[') (char ']') $
    option empty $ do
      head <- term `sepBy1` char ','
      tail <- option empty $ char '|' >> term
      return $ \n -> expandList (fmap ($ n) head) (tail n)
  where
    empty :: Int -> Term
    empty = \_ -> Fun "empty" []

    expandList :: [Term] -> Term -> Term
    expandList []     t = t
    expandList (f:fs) t = Fun "cons" [ f, expandList fs t ]


-- parse a functor: `likes(X,alec)`
functor :: Parser (Int -> Term)
functor = do
      f  <- (:) <$> lower <*> many (alphaNum <|> oneOf "'-_")
      ts <- option [] $ paren (term `sepBy` char ',')
      return $ \n -> Fun f $ fmap ($ n) ts
  where
    paren :: Parser a -> Parser a
    paren p = between (char '(') (char ')') p

-- parse any term, be it variable, symbol, number, functor
term :: Parser (Int -> Term)
term = choice [ list, functor, variable, number ]

-- parse a clause: `likes(X,Y) :- loves(X,Y).`
clause :: Parser (Int -> Clause)
clause = do
  h <- functor
  b <- option [] $ do
         string " :- "
         (functor <|> variable) `sepBy1` string ", "
  char '.'
  return $ \n -> Clause (h n) $ fmap ($ n) b

-- parse a query: `loves(X,alec)?`
query :: Parser Query
query = do
  qs <- (functor <|> variable) `sepBy1` string ", "
  char '?'
  return $ fmap ($ 0) qs

-- parse a program.
program :: Parser Program
program = sepEndBy clause $ many newline
