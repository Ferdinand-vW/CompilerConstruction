-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.HM.Parser
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'Parser' for a simple, implicitly typed functional language.
--
-------------------------------------------------------------------------------

module CCO.HM.Parser (
    -- * Parser
    parser    -- :: Component String Tm
) where

import CCO.HM.AG                     (Var, Tm (Tm), Tm_ (Var, Nat, Lam, App, Let, Prim, Cons, Nil, Head, Tail, If))
import CCO.HM.Lexer                    (Token, lexer, keyword, var, nat, spec)
import CCO.Component                   (Component)
import qualified CCO.Component as C    (parser)
import CCO.Parsing
import Control.Applicative

-------------------------------------------------------------------------------
-- Token parsers
-------------------------------------------------------------------------------

-- | Type of 'Parser's that consume symbols described by 'Token's.
type TokenParser = Parser Token

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

-- A 'Component' for parsing terms.
parser :: Component String Tm
parser = C.parser lexer (pTm <* eof)

-- | Parses a 'Tm'.
pTm :: TokenParser Tm
pTm = (\pos x t1 -> Tm pos (Lam x t1)) <$> sourcePos <* spec '\\' <*> var <* spec '.' <*> pTm <|>
      (\pos x t1 t2 -> Tm pos (Prim x t1 t2)) <$> sourcePos <* keyword "prim" <* spec '\"' <*> var <* spec '\"' <*> pTm <*> pTm <|>
      (\pos t1 -> Tm pos (Head t1)) <$> sourcePos <* keyword "head" <*> pTm <|>
      (\pos t1 -> Tm pos (Tail t1)) <$> sourcePos <* keyword "tail" <*> pTm <|>
      (\pos -> Tm pos Nil) <$> sourcePos <* keyword "nil" <|>
      (\pos x l -> Tm pos (Cons x l)) <$> sourcePos <* keyword "cons" <*> pTm <*> pTm <|>
      (\pos a b c -> Tm pos (If a b c)) <$> sourcePos <* keyword "if" <*> pTm <* keyword "then" <*> pTm <* keyword "else" <*> pTm <* keyword "fi"   <|>
      --(\pos exp true false -> Tm pos (If exp true false)) <$> sourcePos <*> pTm <* keyword "==" <*> pTm <* keyword "then" <*> pTm <* keyword "else" <*> pTm <* keyword "fi" <|>
      (\pos ts -> foldl1 (\t1 t2 -> Tm pos (App t1 t2)) ts) <$>
        sourcePos <*> some
          (
            (\pos x -> Tm pos (Nat x)) <$> sourcePos <*> nat <|>
             (\pos x -> Tm pos (Var x)) <$> sourcePos <*> var <|>
             (\pos x t1 t2 -> Tm pos (Let x t1 t2)) <$> sourcePos <* keyword "let" <*> var <* spec '=' <*> pTm <* keyword "in" <*> pTm <* keyword "ni" <|>
              spec '(' *> pTm <* spec ')'
          )
