{-# LANGUAGE DoAndIfThenElse #-}

module Prolog.Base
  ( Term(..), Clause(..)
  , Program, Query, Substitution
  , query, unify, substitute, variables
  ) where

import Data.Function (on)
import Data.List     (nub)
import Data.Maybe    (maybeToList)


-- TYPES

data Term = Num Integer
          | Var { repr :: String, key :: Int }
          | Fun { name :: String, terms :: [Term]}
  deriving (Eq, Ord)

data Clause = Clause { condition :: Term, body :: [Term] }

type Program = [Int -> Clause]
type Query = [Term]
type Substitution = [(Term,Term)]


-- RESOLUTION

query :: Program -> Query -> [Substitution]
query p qs = do
      θ <- query' 1 p qs
      return $ do
        q <- qs
        x <- variables q
        return $ (x, findVar θ x)
  where
    findVar :: Substitution -> Term -> Term
    findVar θ x@(Var n d) = case lookup x θ of (Just (Var y d))  -> findVar θ (Var y d)
                                               (Just (Fun n ts)) -> Fun n $ fmap (findVar θ) ts
                                               (Just t)          -> t
                                               Nothing           -> Var ("_G" ++ show d ++ n ) d
    findVar _ y = y

    query' :: Int -> Program -> Query -> [Substitution]
    query' d _ [] = []
    query' d p (q:qs) = do
            -- Choose a clause from the program (top to bottom)
            clause <- p
            let (Clause h b) = clause d

            -- Attempt to unify it with the first element in the query
            mgu <- maybeToList $ unify q h

            -- Apply the mgu to (1) remaining elements of query
            --                  (2) body of rule applied
            let b'  = fmap (substitute mgu) b
                qs' = fmap (substitute mgu) qs
                q'  = b' ++ qs' -- new resolvent

            if q' /= [] -- if the resolvent is not empty
              then do s <- query' (d+1) p q'
                      return $ mgu ++ s
            else return $ mgu


-- UNIFICATION

type Stack a = [a]
data State = State { stack :: Stack (Term,Term)
                   , unifier :: Stack (Term,Term) }

unify :: Term -> Term -> Maybe Substitution
unify t t' = unify' $ State [(t,t')] []
  where
    unify' :: State -> Maybe Substitution
    unify' (State [] θ) = Just θ
    unify' (State ((x,y):ts) θ)
      | x `notIn` y   = unify' $ State (subIn x y ts) ((x,y):θ)
      | y `notIn` x   = unify' $ State (subIn y x ts) ((y,x):θ)
      | x == y        = unify' $ State ts θ
      | x `matches` y = unify' $ State (ts ++ terms x `zip` terms y) θ
      | otherwise     = Nothing

    -- checks whether a given variable is not in a term
    notIn v@(Var _ _) t = v `notElem` variables t
    notIn _           _ = False

    -- checks whether two functors can possibly be unified
    matches (Fun f t) (Fun f' t') = f == f' && length t == length t'
    matches _         _           = False

    -- substitutes in every occurence of a variable for a term
    subIn v t = fmap $ uncurry ((,) `on` substitute [(v,t)])


-- GENERAL

-- Extract all the variables from a compound term
-- NOTE: this is lazy!
variables :: Term -> [Term]
variables v@(Var _ _) = [v]
variables (Fun _ ts)  = nub $ concatMap variables ts
variables _           = []

-- Applies a substitution to a term
-- NOTE: you need to substitute into the substitution itself too
substitute :: Substitution -> Term -> Term
substitute [] x = x
substitute ((v@(Var _ _),t):θ) x = substitute (substitute'' v t θ) (substitute' v t x)
  where
    substitute' :: Term -> Term -> Term -> Term
    substitute' v' t' v@(Var _ _) = if v == v' then t' else v
    substitute' v' t' (Fun n t)   = Fun n $ fmap (substitute' v' t') t
    substitute' _  _  t           = t

    substitute'' :: Term -> Term -> Substitution -> Substitution
    substitute'' _  _  []   = []
    substitute'' v' t' ((v,t):θ) = (v,substitute' v' t' t):substitute'' v' t' θ