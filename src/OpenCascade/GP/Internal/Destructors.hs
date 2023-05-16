{-# LANGUAGE CApiFFI #-}
module OpenCascade.GP.Internal.Destructors where

import OpenCascade.GP.Types

import Foreign.Ptr

foreign import capi unsafe "hs_gp_Pnt.h hs_delete_gp_Pnt" deletePnt :: Ptr Pnt -> IO ()
foreign import capi unsafe "hs_gp_Pnt2d.h hs_delete_gp_Pnt2d" deletePnt2d :: Ptr Pnt2d -> IO ()
foreign import capi unsafe "hs_gp_Ax1.h hs_delete_gp_Ax1" deleteAx1 :: Ptr Ax1 -> IO ()
foreign import capi unsafe "hs_gp_Ax2.h hs_delete_gp_Ax2" deleteAx2 :: Ptr Ax2 -> IO ()
foreign import capi unsafe "hs_gp_Ax2.h hs_delete_gp_Ax2" deleteAx2d :: Ptr Ax2d -> IO ()
foreign import capi unsafe "hs_gp_Dir.h hs_delete_gp_Dir" deleteDir :: Ptr Dir -> IO ()
foreign import capi unsafe "hs_gp_Dir2d.h hs_delete_gp_Dir2d" deleteDir2d :: Ptr Dir2d -> IO ()


