module OpenCascade.STEPControl.Writer
( new
, setTolerance
, unsetTolerance
, transfer
, write
) where

import OpenCascade.STEPControl.Internal.Context
import OpenCascade.TopoDS.Internal.Context (topoDSContext)
import OpenCascade.STEPControl.Types (Writer)
import OpenCascade.STEPControl.Internal.Destructors (deleteWriter)
import qualified OpenCascade.TopoDS as TopoDS
import Foreign.C
import Foreign.Ptr
import Data.Acquire
import OpenCascade.Internal.Bool (boolToCBool)
import qualified OpenCascade.IFSelect.ReturnStatus as IFSelect.ReturnStatus
import OpenCascade.STEPControl.StepModelType (StepModelType)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C

C.context (C.cppCtx <> topoDSContext <> stepControlContext)

C.include "<STEPControl_Writer.hxx>"
C.include "<TopoDS_Shape.hxx>"
C.include "<IFSelect_ReturnStatus.hxx>"
C.include "<STEPControl_StepModelType.hxx>"

new :: Acquire (Ptr Writer)
new =
  let createWriter = [C.throwBlock| STEPControl_Writer* {
        return new STEPControl_Writer();
      } |]
  in mkAcquire createWriter deleteWriter

setTolerance :: Ptr Writer -> Double -> IO ()
setTolerance writer tolerance = do
  let cTolerance = realToFrac tolerance
  [C.throwBlock| void {
    $(STEPControl_Writer* writer)->SetTolerance($(double cTolerance));
  } |]

unsetTolerance :: Ptr Writer -> IO ()
unsetTolerance writer = [C.throwBlock| void {
  $(STEPControl_Writer* writer)->UnsetTolerance();
} |]

transfer :: Ptr Writer -> Ptr TopoDS.Shape -> StepModelType -> Bool -> IO IFSelect.ReturnStatus.ReturnStatus
transfer writer shape mode compgraph = do
  let cMode = fromIntegral $ fromEnum mode
      cCompgraph = boolToCBool compgraph
  result <- [C.throwBlock| int {
    return static_cast<int>($(STEPControl_Writer* writer)->Transfer(
      *$(TopoDS_Shape* shape), 
      static_cast<STEPControl_StepModelType>($(int cMode)), 
      $(bool cCompgraph)
    ));
  } |]
  return (toEnum . fromIntegral $ result)

write :: Ptr Writer -> String -> IO IFSelect.ReturnStatus.ReturnStatus
write writer filename = do
  result <- withCString filename $ \cFilename -> [C.throwBlock| int {
    return static_cast<int>($(STEPControl_Writer* writer)->Write($(char* cFilename)));
  } |]
  return (toEnum . fromIntegral $ result)