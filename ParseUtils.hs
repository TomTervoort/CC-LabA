{-# LANGUAGE Haskell2010, FlexibleContexts #-}

-- General helper functions for Parsec parsers.
module ParseUtils where

import Control.Monad
import Data.List
import Data.Maybe
import System.IO
import Debug.Trace
import Data.Functor.Identity

-- parsec3
import Text.Parsec

-- Sequences monads but returns left result. Useful while parsing.
infixl 1 <<
(<<) :: Monad m => m a -> m b -> m a
a << b = do x <- a
            b
            return x

-- Parses any character not equal to the argument.
notChar :: Stream s Identity Char => Char -> Parsec s () Char
notChar c = satisfy (/= c)

-- Similar to 'many', except that every but the last element should be followed by a comma.
commaList :: Stream s Identity Char => Parsec s () a -> Parsec s () [a]
commaList l = option [] $ liftM2 (:) l 
                        $ many (char ',' >> l)
                        
-- Combinator that greedily parses any whitespace and comments in front and after the object to be 
-- parsed. If backtracking is needed, it is recommended to apply 'try' to a parser before ws.
-- Comments start with a %-sign and end with a newline.
-- The parser given as an argument should not expect a (non-optional) '%' or whitespace character 
-- as the first character of the object to be parsed.
-- 'ws (ws x)' is equivalent to 'ws x' and can be safely used.
ws :: Stream s Identity Char => Parsec s () a -> Parsec s () a
ws l = whitespace >> l << whitespace

-- Whitespace eater used by ws.
whitespace :: Stream s Identity Char => Parsec s () ()
whitespace = skipMany (void space <|> void comment)
 where comment = char '%' >> skipMany (notChar '\n') >> char '\n'