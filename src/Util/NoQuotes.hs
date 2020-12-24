module Util.NoQuotes where

newtype NoQuotes = NoQuotes String
    deriving ()

instance Show NoQuotes where 
    show (NoQuotes s) = s