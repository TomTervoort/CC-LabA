{-# LANGUAGE Haskell2010 #-}

-- The parser that converts a BibTeX file into a simple intermediate representation using Parsec.

-- With the subset of BibTeX that we parse, it is never neccessary to look ahead further than a 
-- single character. This means no backtracking is neccessary (Parsec's 'try' is not used) and the
-- parser should be very efficient. We also take advantage of the fact that BibTeX files must be
-- encoded in ASCII, meaning that we can use a quick byte-based aproach with lazy ByteString
-- buffers.
module BibParser (parseBibFile, Identifier, BibEntry (..)) where

import Feedback
import ParseUtils

import Control.Monad
import Data.List
import Data.Maybe
import System.IO
import Debug.Trace

import Data.Map (Map)
import qualified Data.Map as M
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

-- parsec3
import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.ByteString.Lazy

-------------------------------------------------------

-- Represents a BibTeX identifier.
type Identifier = String

-- A single BibTeX entry.
data BibEntry = BibEntry {
                     category   :: Identifier,
                     citeKey    :: Identifier,
                     keyValues  :: [(Identifier, String)]
                   } deriving (Eq, Show)

-- Since BibTeX files are required to be ASCII-encoded, a Parsec parser accepting Lazy Bytestrings
-- that treats input as being encoded as 'char8' (a superset of ASCII) can be used. This is more
-- efficient than a parser consuming a standard String or UTF-16 Text.
type BParser a = Text.Parsec.ByteString.Lazy.Parser a

-- Full parser that can be applied to a complete BibTeX file.
pBib :: BParser [BibEntry]
pBib = do 
 -- Skip everything until next @ or EOF. 
 -- Standard comments are not allowed to contain a @, so this is okay.
 skipMany $ notChar '@'
 atChar <- optionMaybe (char '@')
 if isNothing atChar
  then return [] -- At end of input.
  else do 
   -- Parse entry category.
   cat <- many1 letter
   -- Parse entry, unless category was "Comment".
   if cat == "Comment"
    then pBib -- Note: fails when comment contains a '@' within a string literal. 
                -- Let's just pretend that never happens.
    else do
     ws $ char '{'
     citekey <- (pNumber <|> pName) <?> "Invalid cite key."
     ws $ char ','
     keyVals <- commaList $ ws pField
     char '}'
     rest <- pBib
     return $ BibEntry cat citekey keyVals : rest
     
-- Decimal integer. May be negative.
pNumber :: BParser Identifier
pNumber = (`label` "number") $ liftM2 (:) (digit <|> char '-') $ many digit
    
-- BibTeX identifier.
pName :: BParser Identifier
pName = (`label` "name") $ liftM2 (:) nameChar (many $ nameChar <|> digit)
 where nameChar = letter <|> oneOf "_!$&+./:;<>?^`|"

-- Parses a key-value pair within a BibTeX entry.
pField :: BParser (Identifier, String)
pField = do key <- ws pName
            char '='
            val <- ws pValue
            return (key,val)
              
-- Parses a value. String literals (and any escape sequences therin) are parsed and converted to 
-- the string they represent.
pValue :: BParser String
pValue = (`label` "value") $ pNumber <|> pName <|> pStringLiteral 
             -- No common prefixes, so no backtracking neccessary.
                
-- Consumes a string literal, unquoting and unescaping it.
-- BibTeX supports tons of LateX stuff within string, many of which would which influence style 
-- (e.g. the ^-sign can be used for superscript). Obviously we are not going to cover this and 
-- instead we simply replace a few special characters and account for nested braces.
pStringLiteral :: BParser String
pStringLiteral = (`label` "string literal") $ eatQuotes $ many litUnit
 where eatQuotes p = (char '"' >> p << char '"') <|> brackets p
       brackets p = char '{' >> p << char '}'
       litUnit = brackets (char '"' <|> escapeSeq <|> litUnit) <|> noneOf "{}\""
       
       -- Support non-ASCII characters common in Dutch.
       escapeSeq = do char '\\' 
                      diacritic <- anyChar
                      letter <- anyChar
                      case M.lookup [diacritic, letter] specialChars of
                       Nothing -> fail $ "Unsupported escape sequence: '\\" 
                                          ++ [diacritic, letter] ++ "'."
                       Just c  -> return c 
                       
-- Mapping of a diacritic-letter combination to corresponding Unicode character. Contains pairs 
-- common in Dutch.
specialChars :: Map String Char
specialChars = M.fromList [
                            ("\"a", 'ä'),
                            ("\"e", 'ë'),
                            ("\"o", 'ö'),
                            ("\"u", 'ü'),
                            ("\"i", 'ï'),
                            ("\'a", 'á'),
                            ("\'e", 'é'),
                            ("\'o", 'ó'),
                            ("\'u", 'ú'),
                            ("\'i", 'í'),
                            ("`a", 'à'),
                            ("`e", 'è'),
                            ("`o", 'ò'),
                            ("`u", 'ù'),
                            ("`i", 'ì')
                          ]

-- Tests BParser with file test.bib.
runTest :: IO ()
runTest = do input <- B.readFile "test.bib"
             parseTest pBib input
             
-- Gets a BibTeX file as a lazy Bytestring and tries parsing it.
parseBibFile :: ByteString -> Feedback [BibEntry]
parseBibFile input = case parse pBib "" input of
                      Left  errors -> do forM (errorMessages errors) $ errorF . messageString
                                         fatalF "Unable to parse BibTeX input."
                      Right result -> return result
                      
