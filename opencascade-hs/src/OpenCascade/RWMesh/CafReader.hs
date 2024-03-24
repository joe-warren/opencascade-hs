{-# LANGUAGE CApiFFI #-}
module OpenCascade.RWMesh.CafReader
( CafReader
, setDocument
, perform
, singleShape
, setFileLengthUnit
) where

import OpenCascade.RWMesh.Types (CafReader)
import qualified OpenCascade.TDocStd.Types as TDocStd
import qualified OpenCascade.Message.Types as Message
import qualified OpenCascade.TopoDS.Types as TopoDS
import OpenCascade.TopoDS.Internal.Destructors (deleteShape)
import OpenCascade.Handle (Handle)
import OpenCascade.Internal.Bool (cBoolToBool)
import Foreign.C (CBool (..), CDouble (..))
import Foreign.C.String (CString, withCString)
import Foreign.Ptr (Ptr)
import Data.Coerce (coerce)
import Data.Acquire (Acquire, mkAcquire)

foreign import capi unsafe "hs_RWMesh_CafReader.h hs_RWMesh_CafReader_setDocument" setDocument :: Ptr CafReader -> Ptr (Handle TDocStd.Document) -> IO ()

foreign import capi unsafe "hs_RWMesh_CafReader.h hs_RWMesh_CafReader_setFileLengthUnit" rawSetFileLengthUnit :: Ptr CafReader -> CDouble -> IO ()

setFileLengthUnit :: Ptr CafReader -> Double -> IO ()
setFileLengthUnit = coerce rawSetFileLengthUnit

foreign import capi unsafe "hs_RWMesh_CafReader.h hs_RWMesh_CafReader_perform" rawPerform :: Ptr CafReader -> CString -> Ptr Message.ProgressRange -> IO CBool

perform :: Ptr CafReader -> String -> Ptr Message.ProgressRange -> IO Bool
perform reader filename progress = cBoolToBool <$> (withCString filename $ \str -> rawPerform reader str progress)

foreign import capi unsafe "hs_RWMesh_CafReader.h hs_RWMesh_CafReader_singleShape" rawSingleShape :: Ptr CafReader -> IO (Ptr TopoDS.Shape)

singleShape :: Ptr CafReader -> Acquire (Ptr TopoDS.Shape)
singleShape reader = mkAcquire (rawSingleShape reader) deleteShape

