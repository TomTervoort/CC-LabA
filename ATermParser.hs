{-# LANGUAGE Haskell2010 #-}

-- Parser for an ATerm tree.

module ATermParser (parseATerm, printATerm) where

import ParseUtils
import Feedback

import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import System.IO
import Debug.Trace

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

-- parsec3
import Text.Parsec hiding (runParser)

-- Since ATerm trees are encoded in ASCII, we can use the lazy ByteString parser.
import Text.Parsec.ByteString.Lazy (Parser) 

-- cco
import CCO.Tree (ATerm (..))
import CCO.Printing


-- Parse an ATerm expression. It's possible to parse these with only one character of lookahead, 
-- therefore no backtracking (made explicit by Parsec's 'try' function) is neccessary.
-- All whitespace before and after the expression is consumed.
pATerm :: Parser ATerm
pATerm = do bt  <- ws pBasicTerm
            ann <- ws $ optionMaybe $ pAnnotation
            return $ maybe bt (Ann bt) ann
            
-- Parses an annotation (list of ATerm trees between brackets).
pAnnotation :: Parser [ATerm]
pAnnotation = char '{' >> commaList pATerm << char '}'

pBasicTerm :: Parser ATerm
pBasicTerm = choice [pList, pTuple, pString, pNum, pApp]
 where pList = liftM List $ char '[' >> commaList pATerm << char ']'
       pTuple = liftM Tuple $ char '(' >> commaList pATerm << char ')'
       pString = do char '"'
                    -- Keep consuming string until unescaped ".
                    str <- pString'
                    -- Use Haskells 'read' to unescape string.
                    readM ('"' : str) >>= return . String
       pString' = do c <- anyChar
                     case c of
                      '"'  -> return "\""
                      '\\' -> do c' <- anyChar
                                 rest <- pString'
                                 return $ c : c' : rest
                      _    -> liftM (c:) pString'
       pNum = do -- This is either an integer or a floating point number.
                 -- We only accept simple decimal floats with no exponents.
                 dec1 <- many1 digit
                 -- Check for a dot. If present, this is a float.
                 dot <- optionMaybe $ char '.'
                 if isJust dot
                  then do dec2 <- many1 digit
                          readM (dec1 ++ "." ++ dec2) >>= return . Float
                  else readM dec1 >>= return . Integer
       pApp = do -- Constructor application. 
                 -- Constructor names should match [A-Za-z0-9]+.
                 let alphaNumChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || isDigit c
                 ctor <- many1 $ satisfy alphaNumChar
                 ws $ char '('
                 args <- commaList pATerm
                 char ')'
                 return $ App ctor args
                 
-- Parse an ASCII-encoded ATerm string within the feedback monad.
parseATerm :: ByteString -> Feedback ATerm
parseATerm = runParser pATerm "Invalid ATerm tree."

-- Pretty-prints an ATerm. Tries to keep the width below 80 so it is easy to read.
printATerm :: ATerm -> Text
printATerm = T.pack . render_ 80 . pp 
