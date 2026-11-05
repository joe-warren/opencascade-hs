{-# LANGUAGE CApiFFI #-}
module OpenCascade.BRepAlgoAPI.Internal.Destructors
( deleteFuse
, deleteCut
, deleteCommon
, deleteSection
) where

import OpenCascade.BRepAlgoAPI.Types
import Foreign.Ptr

foreign import capi unsafe "hs_BRepAlgoAPI_Fuse.h hs_delete_BRepAlgoAPI_Fuse" deleteFuse :: Ptr Fuse -> IO ()
foreign import capi unsafe "hs_BRepAlgoAPI_Cut.h hs_delete_BRepAlgoAPI_Cut" deleteCut :: Ptr Cut -> IO ()
foreign import capi unsafe "hs_BRepAlgoAPI_Common.h hs_delete_BRepAlgoAPI_Common" deleteCommon :: Ptr Common -> IO ()
foreign import capi unsafe "hs_BRepAlgoAPI_Section.h hs_delete_BRepAlgoAPI_Section" deleteSection :: Ptr Section -> IO ()
