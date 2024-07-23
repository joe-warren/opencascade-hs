{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepOffsetAPI.Internal.Destructors
( deleteMakePipe
, deleteMakeOffsetShape
, deleteThruSections
) where

import OpenCascade.BRepOffsetAPI.Types

import Foreign.Ptr

foreign import capi unsafe "hs_BRepOffsetAPI_MakePipe.h hs_delete_BRepOffsetAPI_MakePipe" deleteMakePipe :: Ptr MakePipe -> IO ()
foreign import capi unsafe "hs_BRepOffsetAPI_MakeOffsetShape.h hs_delete_BRepOffsetAPI_MakeOffsetShape" deleteMakeOffsetShape :: Ptr MakeOffsetShape -> IO ()
foreign import capi unsafe "hs_BRepOffsetAPI_ThruSections.h hs_delete_BRepOffsetAPI_ThruSections" deleteThruSections :: Ptr ThruSections -> IO ()
