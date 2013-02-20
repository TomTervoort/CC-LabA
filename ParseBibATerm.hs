{-# LANGUAGE Haskell2010 #-}

-- Converts the BibTeX data returned by the BibParser into a textual ATerm representation.
-- Also contains the main function for 'parse-bib'.
module ParseBibATerm where

import Feedback
import BibParser
import ATermParser
import MainModule

import Control.Monad
import Data.List
import Data.Maybe
import System.IO
import Debug.Trace

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding

-- cco
import CCO.Tree (ATerm (..))
import CCO.Printing

-- Convert a list of entries to an appropriate ATerm representation.
bib2ATerm :: [BibEntry] -> ATerm
bib2ATerm = List . map doEntry
 where doEntry entry = App (category entry)
                            $ [
                                App "Key" [String $ citeKey entry],
                                List [App key [String val] | (key,val) <- keyValues entry]
                              ]
							   
-- Main operation. Text output in UTF-8 (ASCII, really).
parse_bib :: ProgramOperation
parse_bib input = parseBibFile input >>= return . encodeUtf8 . printATerm . bib2ATerm

main :: IO ()
main = makeMain parse_bib
                     