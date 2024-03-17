module OpenCascade.IFSelect.ReturnStatus 
( ReturnStatus (..)
) where

-- Should match the order in IFSelect_ReturnStatus.hxx
data ReturnStatus = Void | Done | Error | Fail | Stop deriving (Eq, Enum, Show)