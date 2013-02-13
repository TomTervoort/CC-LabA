{-# LANGUAGE Haskell2010 #-}

-- Contains the definition of the Feedback monad that is intended to contain error and warning
-- side-effects.
module Feedback where 

import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace

import Data.Sequence (Seq, (<|), (|>), (><))
import qualified Data.Sequence as S

-- A simple monad that can store warning and error messages.
data Feedback a = Feedback {
                   warnings :: Seq String,
                   errors   :: Seq String,
                   value    :: Maybe a -- If Nothing, the final error is 'fatal'.
                  } deriving (Show)
                  

-- Add a warning message.
warnF :: String -> Feedback ()
warnF msg = Feedback {warnings = S.singleton msg,
                      errors   = S.empty,
                      value    = Just ()}

-- Add an error message.
errorF :: String -> Feedback ()
errorF msg = Feedback {warnings = S.empty,
                       errors   = S.singleton msg,
                       value    = Just ()}

-- Create a fatal error message that prevents the computation from continuing.
fatalF :: String -> Feedback a
fatalF msg = Feedback {warnings = S.empty,
                       errors   = S.singleton msg,
                       value    = Nothing}
                      
instance Monad Feedback where
 (Feedback w e Nothing) >>= _ = Feedback w e Nothing -- Do not continue after a fatal error.
 (Feedback w1 e1 (Just x)) >>= f = let Feedback w2 e2 y = f x in 
                                    Feedback (w1 >< w2) (e1 >< e2) y
 return x = Feedback S.empty S.empty (Just x)
 fail = fatalF
 
-- Indicates whether an error occured.
hasError :: Feedback a -> Bool
hasError f = not $ S.null $ errors f

-- Returns printed error and warning messages. If none, "" is returned.
printFeedback :: Feedback a -> String
printFeedback f = show $ liftM (const ()) f -- TODO