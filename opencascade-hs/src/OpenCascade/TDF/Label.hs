{-# LANGUAGE CApiFFI #-}
module OpenCascade.TDF.Label
( Label
, isNull
) where

import OpenCascade.TDF.Types (Label)
import OpenCascade.Internal.Bool (cBoolToBool)
import Foreign.Ptr (Ptr)
import Foreign.C (CBool (..))

foreign import capi unsafe "hs_TDF_Label.h hs_TDF_Label_isNull" rawIsNull :: Ptr Label -> IO CBool

isNull :: Ptr Label -> IO Bool
isNull = fmap cBoolToBool . rawIsNull
