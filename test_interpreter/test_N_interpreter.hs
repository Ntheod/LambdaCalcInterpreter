module Skeleton where

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.List
import Data.Char
import Text.Parsec
import Text.Parsec.String
import qualified Text.PrettyPrint as PP

data Term = Var String
		  | Application Term Term
		  | Abstraction String Term
		  deriving(Show,Eq)

data Result = Res Term Int [Term] [String] deriving(Show,Eq)

-------------------- PARSER --------------------------------

lambdaTerm :: Parser Term
lambdaTerm = lambdaAbstraction <|> lambdaApplication <|> simple

lambdaAbstraction :: Parser Term
lambdaAbstraction = do
  	char '\\'
  	var <- letter
  	char '.'
  	body <- lambdaTerm
  	return(Abstraction [var] body)

lambdaApplication :: Parser Term
lambdaApplication = do
  apps <- many1 simple
  return(foldl1 Application apps)

simple :: Parser Term
simple = lambdaVar <|> paren

lambdaVar :: Parser Term
lambdaVar = do
  var <- letter
  return(Var [var])

paren :: Parser Term
paren = do
  char '('
  term <- lambdaTerm
  char ')'
  return term

myparse :: String -> Term
myparse str = case (parse lambdaTerm "" str) of
	Left msg -> error $ show msg
	Right term' -> term'

test = myparse "\\z.(\\f.\\x.fzx)(\\y.y)"
pair = myparse "\\x.\\y.\\z.zxy"

----------------------- PRETTY PRINT -----------------------

ppr :: Term -> PP.Doc
ppr (Var x) = PP.text x
ppr (Abstraction x e) = PP.fcat [(PP.fcat [PP.text "\\",PP.text x,PP.text "."]),(ppr e)]
ppr apply = PP.fcat (map parenApp (args apply))


args (Application x y) = args x ++ [y]
args x = [x]

parenApp (x@(Application _ _)) = PP.parens (ppr x)
parenApp (x@(Abstraction _ _)) = PP.parens (ppr x)
parenApp x = ppr x

prettyprint :: Term -> String
prettyprint term = PP.render (ppr term)

------------------------ TEST CASES ------------------------

inputString = "\\x.(\\y.x)(\\z.z)"

parseInputString = myparse inputString

myterm = Application (Abstraction "x" ( Abstraction "y"  (Var "x"))) (Abstraction "z" ( Var "z"))

prettyPrinted = prettyprint myterm
------------------------ REDUCE ----------------------------

checkVar :: Term -> Bool
checkVar t@(Var _) = True
checkVar t@(Application _ _) = False
checkVar t@(Abstraction _ _) = False

checkAbs :: Term -> Bool
checkAbs t@(Var _) = False
checkAbs t@(Application _ _) = False
checkAbs t@(Abstraction _ _) = True
------ BETA ------
finds :: String -> Term -> Term -> Term
finds s lterm@(Var v) rterm = case v==s of
                      True -> rterm
                      False -> lterm
finds s lterm@(Abstraction s1 x) rterm = (Abstraction s1 (finds s x rterm))
finds s lterm@(Application x y) rterm = (Application (finds s x rterm) (finds s y rterm))
  
  
replace :: Result -> Result -> Result
replace (lterm@(Abstraction s x) counter arlist redxlist) (rterm counter arlist redxlist) = resbeta (finds s x rterm) counter arlist redxlist
replace (lterm@(Application x y) counter arlist redxlist) (rterm counter arlist redxlist) = case (checkVar x) of
                                  True -> ((Application (Application x y) (resbeta rterm counter arlist redxlist)) counter arlist redxlist)
                                  False -> resbeta (Application (replace x y) rterm) counter arlist redxlist

{-beta :: Term -> Term
beta lterm@(Application x y) = case (checkVar x) of
                                    True -> (Application x (beta y))
                                    False -> replace x y
beta lterm@(Abstraction s x) = (Abstraction s (beta x))
beta lterm@(Var s) = (Var s)-}

resbeta :: Result -> Result
resbeta lterm@(Application x y) counter arlist redxlist = case (checkVar x) of
                                    True -> ((Application x (resbeta y counter arlist redxlist))) counter arlist redxlist)
                                    False -> resbeta (replace x y) succ(counter) (arlist++[(replace x y)]) (redxlist++[beta])
resbeta lterm@(Abstraction s x) counter arlist redxlist = ((Abstraction s (resbeta x counter arlist redxlist))) counter arlist redxlist)
resbeta lterm@(Var s) counter arlist redxlist = ((Var s) counter arlist redxlist)

------ ETA -----

findlast :: String -> Term -> Bool
findlast s lterm@(Var v) = case v == s of
                        True -> True
                        False -> False
findlast s lterm@(Application x y) = findlast s y
findlast s lterm@(Abstraction s1 x) = findlast s x

remove :: Term -> Term
remove lterm@(Application x y) = case (checkVar y) of
                                  True -> x
                                  False -> (Application x (remove y))
remove lterm@(Abstraction s x) = case (checkVar x) of
                                  True -> lterm
                                  False -> (Abstraction s (remove x))

eta :: Term -> Term
eta lterm@(Application x y) = lterm
eta lterm@(Var s) = lterm
eta lterm@(Abstraction s x) = case (checkVar x) of
                        True -> lterm
                        False -> case (findlast s x) of
                                  True -> (remove x)
                                  False -> case (checkAbs x) of
                                            True -> (Abstraction s (eta x))
                                            False -> lterm

reduce :: Term -> Term
reduce lterm = (eta(beta lterm))