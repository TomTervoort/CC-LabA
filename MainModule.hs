{-# LANGUAGE Haskell2010 #-}

-- Contains the command line argument parser and feedback displayer that can be used for the main
-- modules of the executables.
module MainModule where

import Feedback

import Control.Monad
import Data.List
import Data.Maybe
import System.IO
import Debug.Trace
import System.Environment

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as TIO

-- The actions performed by each executable can be characterised by this type: input is read from
-- some handle and the resulting output is returned within a lazy ByteString containing ASCII text, 
-- along with feedback.
type ProgramOperation = ByteString -> Feedback Text

-- Creates a 'main' function from a ProgramOperation.
makeMain :: ProgramOperation -> IO ()
makeMain op = do args <- getArgs
                 -- Use the argument as input file name, or use stdin when there are no arguments.
                 input <- case args of
                           []     -> B.getContents
                           [path] -> B.readFile path
                 let feedback = op input
                 hPutStrLn stderr $ printFeedback feedback
                 when (not $ hasError feedback) $ TIO.putStrLn $ fromJust $ value feedback