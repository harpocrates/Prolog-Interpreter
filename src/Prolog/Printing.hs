module Prolog.Printing
  ( showProgram, showQuery, showSubstitution
  ) where

import Prolog.Base

import Data.List  (intercalate)
import Data.Maybe (catMaybes)

instance Show Term where
  show (Var v _)  = v
  show (Num n)    = show n

  show f = head $ catMaybes [ show <$> toNumber f,   -- verify `f` is of the form `s(..s(zero))..)`
                              showList <$> toList f, -- verify `f` is a `cons`
                              Just $ showDefault f ] -- other cases
    where
      showList :: ([Term], Maybe Term) -> String
      showList (l, r) = "[" ++ intercalate "," (show <$> l) ++ r' ++ "]"
        where r' = case r of Nothing -> ""
                             Just t  -> "|" ++ show t

      showDefault :: Term -> String
      showDefault (Fun n []) = n
      showDefault (Fun n t)  = n ++ "(" ++ intercalate "," (fmap show t) ++ ")"

      toList :: Term -> Maybe ([Term], Maybe Term)
      toList (Fun "empty" [])   = Just ([], Nothing)
      toList (Fun "cons" [f,r]) = Just $ case toList r of Nothing -> ([f],Just r)
                                                          Just (r',d) -> (f:r',d)
      toList _ = Nothing

      toNumber :: Term -> Maybe Integer
      toNumber (Fun "zero" []) = Just 0
      toNumber (Fun "s" [n]) = (+1) <$> toNumber n
      toNumber _ = Nothing


instance Show Clause where
  show (Clause h []) = show h ++ "."
  show (Clause h b)  = show h ++ " :- " ++ intercalate ", " (map show b) ++ "."


showProgram :: Program -> String
showProgram p = unlines $ fmap (\c -> show $ c 0) p

showQuery :: Query -> String
showQuery qs = intercalate ", " (fmap show qs) ++ "?\n"

showSubstitution :: Substitution -> String
showSubstitution θ = "{ " ++ intercalate ", " (map showPair θ) ++ " }"
  where
    showPair (Var x _, t) = x ++ " = " ++ show t
