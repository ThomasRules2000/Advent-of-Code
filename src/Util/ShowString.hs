module Util.ShowString where
import           Prelude hiding (showString)

class Show a => ShowString a where
    showString :: a -> String
    showString = show

    printString :: a -> IO ()
    printString = putStrLn . showString

instance ShowString String where
    showString s = s

instance ShowString Int

instance ShowString Char

instance ShowString Bool
