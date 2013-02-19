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
import System.Exit

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B


-- The actions performed by each executable can be characterised by this type: a sequence of bytes 
-- (containing text encoded in either ASCII or UTF-8) is read from stdin or a file and transformed 
-- into another byte sequence that is to be send to standard out. The Bytestring are lazy.
type ProgramOperation = ByteString -> Feedback ByteString

-- Creates a 'main' function from a ProgramOperation.
makeMain :: ProgramOperation -> IO ()
makeMain op = do args <- getArgs
                 -- Use the argument as input file name, or use stdin when there are no arguments.
                 input <- case args of
                           []     -> B.getContents
                           [path] -> B.readFile path
                 let feedback = op input
                 case runFeedback feedback of
                  (Just outp, msg) | not $ hasError feedback -> 
                                        do -- Dump warnings, if any.
                                           when (msg /= "") $ hPutStrLn stderr msg
                                           -- Print output.
                                           B.putStr outp
                  (_, msg)           -> do -- Compilation failed. Print errors/warnings.
                                           hPutStrLn stderr msg
                                           -- Terminate with an exit code indicating failure.
                                           exitFailure