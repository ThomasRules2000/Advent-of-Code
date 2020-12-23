module Util.Field where
  import Data.IntSet

  data Field = Field { fieldName :: String,
                       fieldSet  :: IntSet
                     } deriving ()

  instance Show Field where
    show f = "\"" ++ fieldName f ++ "\""
