{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepFilletAPI.Internal.Destructors
( deleteMakeFillet
, deleteMakeChamfer
) where

import OpenCascade.BRepFilletAPI.Types

import Foreign.Ptr

foreign import capi unsafe "hs_BRepFilletAPI_MakeFillet.h hs_delete_BRepFilletAPI_MakeFillet" deleteMakeFillet :: Ptr MakeFillet -> IO ()

foreign import capi unsafe "hs_BRepFilletAPI_MakeChamfer.h hs_delete_BRepFilletAPI_MakeChamfer" deleteMakeChamfer :: Ptr MakeChamfer -> IO ()


