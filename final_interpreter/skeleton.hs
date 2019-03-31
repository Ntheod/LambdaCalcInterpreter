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
------------------------ ASKISI 3 ----------------------------

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
  
  
replace :: Term -> Term -> Term
replace lterm@(Abstraction s x) rterm = finds s x rterm
replace lterm@(Application x y) rterm = case (checkVar x) of
                                  True -> (Application (Application x y) rterm)
                                  False -> (Application (replace x y) rterm)

beta :: Term -> Term
beta lterm@(Application x y) = case (checkVar x) of
                                    True -> (Application x (beta y))
                                    False -> replace x y
beta lterm@(Abstraction s x) = (Abstraction s (beta x))
beta lterm@(Var s) = (Var s)

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

------ REDUCE ------

resredx :: Result -> Result
resredx aresult@(Res lterm counter termlst strlst) = case lterm == (beta lterm) of
                                              True -> case lterm == (eta lterm) of
                                                      True -> aresult
                                                      False -> (resredx (Res (eta lterm) (succ(counter)) (termlst++[(eta lterm)]) (strlst++["eta"])))
                                              False ->(resredx (Res (beta lterm) (succ(counter)) (termlst++[(beta lterm)]) (strlst++["beta"])))

reduce :: Term -> Result
reduce lterm = resredx (Res lterm 0 [] [])