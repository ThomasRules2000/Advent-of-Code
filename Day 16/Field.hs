module Field where
  import Data.Set

  data Field = Field { fieldName :: String,
                       fieldSet  :: Set Int
                     } deriving ()

  instance Show Field where
    show f = "\"" ++ fieldName f ++ "\""
