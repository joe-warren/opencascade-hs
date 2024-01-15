{-# LANGUAGE CApiFFI #-}
module OpenCascade.GP.Internal.Destructors
( deletePnt
, deletePnt2d
, deleteAx1
, deleteAx2
, deleteAx2d
, deleteAx3
, deleteDir
, deleteDir2d
, deleteVec
, deleteVec2d
, deleteTrsf
, deleteTrsf2d
, deleteGTrsf
) where

import OpenCascade.GP.Types

import Foreign.Ptr

foreign import capi unsafe "hs_gp_Pnt.h hs_delete_gp_Pnt" deletePnt :: Ptr Pnt -> IO ()
foreign import capi unsafe "hs_gp_Pnt2d.h hs_delete_gp_Pnt2d" deletePnt2d :: Ptr Pnt2d -> IO ()
foreign import capi unsafe "hs_gp_Ax1.h hs_delete_gp_Ax1" deleteAx1 :: Ptr Ax1 -> IO ()
foreign import capi unsafe "hs_gp_Ax2.h hs_delete_gp_Ax2" deleteAx2 :: Ptr Ax2 -> IO ()
foreign import capi unsafe "hs_gp_Ax2d.h hs_delete_gp_Ax2d" deleteAx2d :: Ptr Ax2d -> IO ()
foreign import capi unsafe "hs_gp_Ax3.h hs_delete_gp_Ax3" deleteAx3 :: Ptr Ax3 -> IO ()
foreign import capi unsafe "hs_gp_Dir.h hs_delete_gp_Dir" deleteDir :: Ptr Dir -> IO ()
foreign import capi unsafe "hs_gp_Dir2d.h hs_delete_gp_Dir2d" deleteDir2d :: Ptr Dir2d -> IO ()
foreign import capi unsafe "hs_gp_Vec.h hs_delete_gp_Vec" deleteVec :: Ptr Vec -> IO ()
foreign import capi unsafe "hs_gp_Vec2d.h hs_delete_gp_Vec2d" deleteVec2d :: Ptr Vec2d -> IO ()
foreign import capi unsafe "hs_gp_Trsf.h hs_delete_gp_Trsf" deleteTrsf :: Ptr Trsf -> IO ()
foreign import capi unsafe "hs_gp_GTrsf.h hs_delete_gp_GTrsf" deleteGTrsf :: Ptr GTrsf -> IO ()
foreign import capi unsafe "hs_gp_Trsf2d.h hs_delete_gp_Trsf2d" deleteTrsf2d :: Ptr Trsf2d -> IO ()

