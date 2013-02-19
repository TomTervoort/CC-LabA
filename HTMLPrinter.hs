{-# LANGUAGE Haskell2010 #-}

-- Transforms an ATerm tree representing HTML into a pretty printed HTML document. 
-- Uses Text.PrettyPrint.
module HTMLPrinter (main) where

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

-- pretty
import Text.PrettyPrint

-- Pretty print a HTML page represented through an ATerm.
-- Errors are logged in case the tree is invalid.
-- Four spaces of identation are added each level.
ppHTML :: ATerm -> Feedback Doc
ppHTML (Ann (App tag [List children]) atts) = 
 do attDocs <- forM atts ppAttribute
    childDocs <- forM children ppHTML
    return $ openTag tag attDocs $+$ nest 4 (vcat childDocs) $+$ closeTag tag
ppHTML c@(App _ _) = ppHTML $ Ann c []
ppHTML (String txt) = return $ text $ htmlEscape txt
ppHTML term = errorF ("Can not convert to HTML: '" ++ show term ++ "'.") >> return empty

openTag :: String -> [Doc] -> Doc
openTag tag atts = char '<' <> text (map toLower tag) <> hsep atts <> char '>'

closeTag :: String -> Doc
closeTag tag = char '<' <> text (map toLower tag) <> text "/>"

-- Pretty prints a tag attribute.
ppAttribute :: ATerm -> Feedback Doc
ppAttribute (App key [String val]) = return $ text key <+> char '=' <+> text (htmlEscape val)
ppAttribute t = errorF ("Invalid attribute: '" ++ show t ++ "'.") >> return empty

-- Replaces all special characters (control characters, non-ASCII characters, characters that 
-- conflict with HTML syntax) with their HTML escape sequences.
htmlEscape :: String -> String
htmlEscape = concatMap escapeChar
 where escapeChar c | c < chr 32 || c > chr 126 || c `elem` "&<>\"'/" = "&#" ++ show (ord c)
                    | otherwise = [c]
                    
-- The printing as a ProgramOperation.
-- Assume default page style.
printHTML :: ProgramOperation
printHTML inp = do aterm <- parseATermUtf8 inp
                   doc   <- ppHTML aterm
                   return $ encodeUtf8 $ T.pack $ (++ "\n") $ render doc
                   
main :: IO ()
main = makeMain printHTML