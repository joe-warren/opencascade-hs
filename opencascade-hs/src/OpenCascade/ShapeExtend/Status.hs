module OpenCascade.ShapeExtend.Status 
( Status (..)
) where

-- This needs to match the definition in ShapeExtend_Status.hxx
data Status = 
    OK |
    DONE1 |
    DONE2 |
    DONE3 |
    DONE4 |
    DONE5 |
    DONE6 |
    DONE7 |
    DONE8 |
    DONE |
    FAIL1 |
    FAIL2 |
    FAIL3 |
    FAIL4 |
    FAIL5 |
    FAIL6 |
    FAIL7 |
    FAIL8 |
    FAIL deriving (Show, Eq, Enum)