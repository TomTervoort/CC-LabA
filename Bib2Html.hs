{-# LANGUAGE Haskell2010 #-}

-- Transforms an ATerm tree representing BibTex into an ATerm tree representing an HTML document. 
module Bib2Html (main) where

import Feedback
import ATermParser
import MainModule

import Control.Monad
import Data.List
import Data.Maybe
import System.IO
import Data.Char

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding

import CCO.Tree (ATerm (..))
import CCO.Printing

-- the main processing initiator
process :: ATerm -> Feedback ATerm
process tree = validator tree >>= return . toHtml . sortEntries

-- Sort the entries first by author, then by year and last by title
sortEntries :: ATerm -> [ATerm]
sortEntries l@(List entries) = sortBy sorter entries
 where sorter e1 e2
        | (getVariable "author" e1) == (getVariable "author" e2) && (getVariable "year" e1) == (getVariable "year" e2) = compare (getVariable "title" e1) (getVariable "title" e2)
        | (getVariable "author" e1) == (getVariable "author" e2) = compare (getVariable "year" e1) (getVariable "year" e2)
        | otherwise = compare (getVariable "author" e1) (getVariable "author" e2)

-- the actual BibTex to HTML ATerm conversion
toHtml :: [ATerm] -> ATerm
toHtml entries = App "HTML" [List [
                     App "HEAD" [List [
                         App "TITLE" [List [
                             String "Bibliography"
                         ]]
                     ]],
                     App "BODY" [List ((outputContents entries) ++
                         [App "HR" [List []]] ++ (outputTable entries))]
                     ]]

-- converts an ATerm description of a BibTex entry to a referencable code
getCode :: ATerm -> String
getCode e = (concat . map (\x -> [head x]) $ filter (\x -> last x == ',') $ words (getVariable "author" e)) ++ 
            (reverse . take 2 . reverse $ (getVariable "year" e))

-- given an ATerm description of a BibTex entry, get a variable (like author, title, etc)
getVariable :: String -> ATerm -> String
getVariable s (App _ l) = get_val (find sat_s (get_list (l!!1)))
                          where get_list (List entries) = entries
                                get_list _ = [] 
                                sat_s (App s2 l) | s == s2 = True
                                sat_s _ = False
                                get_val :: Maybe ATerm -> String
                                get_val (Just(App s2 ((String v):xs))) = v
                                get_val _ = ""

-- given an ATerm description of a BibTex entry, return if a variable is defined
hasVariable :: String -> ATerm -> Bool
hasVariable s x = not $ getVariable s x == ""

-- outputs the table of contents
outputContents :: [ATerm] -> [ATerm]
outputContents (x@(App typ l):xs) = Ann (App "A" [List [String ("[" ++ (getCode x) ++ "]")]]) 
                                    [(App "href" [String (get_key (l !! 0))])] : (String sep) : (outputContents xs)
                                    where sep | xs == []  = ""
                                              | otherwise = " | " 
outputContents [] = []

-- outputs the HTML table
outputTable :: [ATerm] -> [ATerm]
outputTable e = [Ann (App "TABLE" [ List (rows e)]) [(App "border" [String "0"])]]
                where rows (x:xs) = Ann (App "TR" [List (outputLink x ++ outputRef x)]) [(App "valign" [String "top"])] : rows xs
                      rows [] = []

-- outputs the link side of the table (left side)
outputLink :: ATerm -> [ATerm]
outputLink x@(App typ l) = [App "TD" [List [
                             Ann (App "A" [List [String ("[" ++ (getCode x) ++ "]")]]) [(App "name" [String (get_key (l !! 0))])]
                           ]]]

-- outputs the actual reference (right side)
outputRef :: ATerm -> [ATerm]
outputRef x@(App typ l) = [App "TD" [List (
                            at "author" [String (getVariable "author" x), String(".")] ++
                            at "title" (wrapon "EM" (not $ hasVariable "booktitle" x) [String (getVariable "title" x), String (".")]) ++
                            at "booktitle" [String (" In: ")] ++
                            at "editor" [String (getVariable "editor" x),
                                         String (", editors, ")] ++
                            at "booktitle" (wrap "EM" [String (getVariable "booktitle" x)]) ++
                            at "pages" [String (", pages "), String (getVariable "pages" x), 
                                        String (". ")] ++
                            at "publisher" [String (getVariable "publisher" x), String (", ")] ++
                            at "address" [String (getVariable "address" x), String (", ")] ++
                            at "year" [String (getVariable "year" x), String (".")]
                          )]]
                          where at s xs = if (hasVariable s x) then xs else []
                                wrap tag xs = [App tag [List ( xs )]]
                                wrapon tag True xs = wrap tag xs
                                wrapon tag False xs = xs

-- given an ATerm description of a BibTex entry, get the name of this entry
get_key :: ATerm -> String
get_key (App "Key" ((String x):xs)) = x
get_key _ =  "Unknown"

-- The validator validates if the required variables are actually there. If one is not, we will print out a warning
validator :: ATerm -> Feedback ATerm
validator l@(List entries) = do entries <- forM entries validator
                                return l
validator c@(App "article" cont)       = validate_type ["author", "title", "journal", "year"] cont >> return c
validator c@(App "book" cont)          = validate_type ["author", "title", "publisher", "year"] cont >> return c
validator c@(App "booklet" cont)       = validate_type ["title"] cont >> return c
validator c@(App "inbook" cont)        = validate_type ["author", "title", "publisher", "year"] cont >> return c
validator c@(App "incollection" cont)  = validate_type ["author", "title", "booktitle", "publisher", "year"] cont >> return c
validator c@(App "inproceedings" cont) = validate_type ["author", "title", "booktitle", "year"] cont >> return c
validator c@(App "manual" cont)        = validate_type ["title"] cont >> return c
validator c@(App "mastersthesis" cont) = validate_type ["author", "title", "school", "year"] cont >> return c
validator c@(App "misc" cont)          = validate_type [] cont >> return c
validator c@(App "phdthesis" cont)     = validate_type ["author", "title", "school", "year"] cont >> return c
validator c@(App "proceedings" cont)   = validate_type ["author", "year"] cont >> return c
validator c@(App "techreport" cont)    = validate_type ["author", "title", "institution", "year"] cont >> return c
validator c@(App "unpublished" cont)   = validate_type ["author", "title", "note"] cont >> return c
validator a = fatalF("Unrecognized input") >> return a -- should never happen, but just in case :)

-- checks if a BibTex entry defines a variable. If it doesn't, throw a warning.
validate_type :: [String] -> [ATerm] -> Feedback [ATerm]
validate_type (x:xs) cont = do if any (is_con x) (get_list (cont !! 1)) then
                                   validate_type xs cont
                               else
                                   warnF("Entry '" ++ (get_key (cont !! 0)) ++ "' does not have required variable '" ++ x ++ "'")  >> validate_type xs cont
                            where get_list :: ATerm -> [ATerm]
                                  get_list (List entries) = entries
                                  get_list _ = []                  
                                  is_con s1 (App s2 _) | s1 == s2 = True
                                  is_con _  _                     = False     
validate_type [] cont = return cont

bib2html :: ProgramOperation
bib2html inp = do aterm <- parseATerm inp
                  out   <- process aterm
                  return $ encodeUtf8 $ printATerm out

-- Pretty-prints an ATerm. Tries to keep the width below 80 so it is easy to read.
printATerm :: ATerm -> Text
printATerm = T.pack . render_ 80 . pp 

main :: IO ()
main = makeMain bib2html