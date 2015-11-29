-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Diag.Lexer
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'Lexer' for 'Diag's.
--
-------------------------------------------------------------------------------

module CCO.Diag.Lexer (
    -- * Tokens
    Token      -- abstract, instance: Symbol

    -- * Lexer
  , lexer      -- :: Lexer Token

    -- * Token parsers
  , keyword    -- :: String -> Parser Token String
  , ident      -- :: Parser Token Ident
  ) where

import CCO.Diag.Base                (Ident)
import CCO.Lexing hiding (satisfy)
import CCO.Parsing                  (Symbol (describe), Parser, satisfy, (<!>))
import Control.Applicative          (Alternative ((<|>)), (<$>), some)

-------------------------------------------------------------------------------
-- Tokens
-------------------------------------------------------------------------------

-- | Type of 'Diag' tokens.
data Token
  = Keyword { fromKeyword :: String }    -- ^ Keyword.
  | Ident   { fromIdent   :: Ident  }    -- ^ Identifier.

instance Symbol Token where
  describe (Keyword _) lexeme = "keyword "    ++ lexeme
  describe (Ident _)   lexeme = "identifier " ++ lexeme

-- | Retrieves whether a 'Token' is a 'Keyword'.
isKeyword :: Token -> Bool
isKeyword (Keyword _) = True
isKeyword _           = False

-- | Retrieves whether a 'Token' is an 'Ident'.
isIdent :: Token -> Bool
isIdent (Ident _) = True
isIdent _         = False

-------------------------------------------------------------------------------
-- Lexer
-------------------------------------------------------------------------------

-- | A 'Lexer' that recognises (and ignores) whitespace.
layout_ :: Lexer Token
layout_ = ignore (some (anyCharFrom " \n\t"))

-- | A 'Lexer' that recognises 'Keyword' tokens.
keyword_ :: Lexer Token
keyword_ = fmap Keyword $ string "compiler" <|> string "compile" <|>
                          string "end" <|> string "execute" <|>
                          string "for" <|> string "from" <|> string "in" <|>
                          string "interpreter" <|> string "on" <|>
                          string "platform" <|> string "program" <|>
                          string "to" <|> string "with"

-- | A 'Lexer' that recognises 'Ident' tokens.
ident_ :: Lexer Token
ident_ = Ident <$> some (anyCharBut " \n\t")

-- | A 'Lexer' for 'Diag's.
lexer :: Lexer Token
lexer = layout_ <|> keyword_ <|> ident_

-------------------------------------------------------------------------------
-- Token parsers
-------------------------------------------------------------------------------

-- | A 'Parser' that recognises a specified keyword.
keyword :: String -> Parser Token String
keyword key = fromKeyword <$>
              satisfy (\tok -> isKeyword tok && fromKeyword tok == key) <!>
              "keyword " ++ key

-- | A 'Parser' that recognises an identifier.
ident :: Parser Token Ident
ident = fromIdent <$> satisfy isIdent <!> "identifier"