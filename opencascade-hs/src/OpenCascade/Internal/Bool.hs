module OpenCascade.Internal.Bool
( boolToCBool 
, cBoolToBool
) where


import Foreign.C

boolToCBool :: Bool -> CBool 
boolToCBool b = if b then 1 else 0

cBoolToBool :: CBool -> Bool
cBoolToBool = (/=0)

