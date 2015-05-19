module HProlog.HProlog where

import Data.Maybe

type UpperCaseString = String
type LowerCaseString = String


data Term = Var UpperCaseString Int
     | Func LowerCaseString [Term]
     | Atom LowerCaseString
     deriving (Show, Eq)

infix 6 :-
data Clause = Term :- [Term]
    deriving (Show, Eq)

type Rules = [Clause]

-- Result is a list of variables to atoms mappings.
type Substitution = [(Term, Term)]
data Result = Result [Substitution]
        deriving (Show, Eq)


solve :: Rules -> Term -> Result
solve rules goal = Result (findSubstitutions rules 1 [goal])

findSubstitutions :: Rules -> Int -> [Term] -> [Substitution]
findSubstitutions _ _ [] = [[]] -- [[]] means true/success
findSubstitutions rules i goals =
            let rules' = rename rules i in
                [
                    s ++ s' |
                    (s, goals') <- branch rules' goals,
                    s'          <- findSubstitutions rules (i + 1) goals'
                ]

--returns a list of substitutions and a list of terms that have to be unified in the next level of the tree
branch :: Rules -> [Term] -> [(Substitution, [Term])]
branch _ [] = []
branch rules (goal:goals) = [
            (s, applySubstitutions s (body ++ goals)) |
                head :- body <- rules, -- pentru fiecare regula
                s <- maybeToList (unify goal head) -- unificam termenul scop cu primul termen
            ]


rename :: Rules -> Int -> Rules
rename rules i = [ renameVar head :- renameVars body | head :- body <- rules]
        where
          renameVar (Var s _)        = Var s i
          renameVar (Atom a)         = Atom a
          renameVar (Func name args) = Func name (renameVars args)

          renameVars = map renameVar


-- the second argument is the list of goals
-- the return value is their values
-- applySubstitutions [(Var "X" 1, Atom "abc")] [Func "test" [Var "X" 1, Var "Y" 1]] -> [Func "test" [Atom "abc", Var "Y" 1]]
applySubstitutions :: Substitution -> [Term] -> [Term]
applySubstitutions subs = map (substituteTerm subs)


-- i and j denote the level of depth and are used to check for the same context
substituteTerm  :: Substitution -> Term -> Term
substituteTerm []                  t     = t
substituteTerm ((Var x i, t):subs) term@(Var y j)
                                | x == y && i == j  = substituteTerm subs t
                                | otherwise         = substituteTerm subs term
substituteTerm _                   (Atom val)       = Atom val
substituteTerm subs                f@(Func name args) = Func name (applySubstitutions subs args)

substituteTerm (_:subs)   v@(Var _ _)   = substituteTerm subs v


unify :: Term -> Term -> Maybe Substitution
unify (Atom a) (Atom b)
                    | a == b    = Just []
                    | otherwise = Nothing
unify v1@(Var _ _) v2@(Var _ _) = Just [(v1, v2)]
unify v@(Var _ _)  b            = Just [(v, b)]
unify a            v@(Var _ _)  = Just [(v, a)]

unify (Func name args) (Func name2 args2)
                    | name == name2 = unifyList args args2
                    | otherwise     = Nothing
unify _ _ = Nothing

unifyList :: [Term] -> [Term] -> Maybe Substitution
unifyList [] [] = Just []
unifyList _ []  = Nothing
unifyList [] _  = Nothing
unifyList (x:xs) (y:ys) =
        do s <- unify x y
           s' <- unifyList (applySubstitutions s xs) (applySubstitutions s ys)
           return (s ++ s')
