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

import CCO.HM.AG                     (Var, Tm (Tm), Tm_ (HVar, HNat, HLam, HApp, HLet,HPrim))
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
pTm = (\pos x t1 -> Tm pos (HLam x t1)) <$> sourcePos <* spec '\\' <*> var <* spec '.' <*> pTm <|>
      (\pos x t1 t2 -> Tm pos (HPrim x t1 t2)) <$> sourcePos <* keyword "prim" <*> var <*> pTm <*> pTm <|>
      (\pos ts -> foldl1 (\t1 t2 -> Tm pos (HApp t1 t2)) ts) <$>
        sourcePos <*> some
          (
            (\pos x -> Tm pos (HNat x)) <$> sourcePos <*> nat <|>
             (\pos x -> Tm pos (HVar x)) <$> sourcePos <*> var <|>
             (\pos x t1 t2 -> Tm pos (HLet x t1 t2)) <$> sourcePos <* keyword "let" <*> var <* spec '=' <*> pTm <* keyword "in" <*> pTm <* keyword "ni" <|>
              spec '(' *> pTm <* spec ')'
          )
