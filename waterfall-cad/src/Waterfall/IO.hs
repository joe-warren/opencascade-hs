module Waterfall.IO
( ReadError (..)
, writeSTL
, writeSTEP
, writeGLTF
, writeGLB
, readSTL
) where 

import Waterfall.Internal.Solid (Solid(..))
import qualified OpenCascade.BRepMesh.IncrementalMesh as BRepMesh.IncrementalMesh
import qualified OpenCascade.StlAPI.Writer as StlWriter
import qualified OpenCascade.StlAPI.Reader as StlReader
import qualified OpenCascade.STEPControl.Writer as StepWriter
import qualified OpenCascade.STEPControl.StepModelType as StepModelType
import qualified OpenCascade.TDocStd.Document as TDocStd.Document
import qualified OpenCascade.Message.ProgressRange as Message.ProgressRange
import qualified OpenCascade.TColStd.IndexedDataMapOfStringString as TColStd.IndexedDataMapOfStringString
import qualified OpenCascade.RWGltf.CafWriter as RWGltf.CafWriter
import qualified OpenCascade.XCAFDoc.DocumentTool as XCafDoc.DocumentTool
import qualified OpenCascade.XCAFDoc.ShapeTool as XCafDoc.ShapeTool
import qualified OpenCascade.TopoDS.Types as TopoDS
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import qualified OpenCascade.ShapeFix.Solid as ShapeFix.Solid
import qualified OpenCascade.ShapeExtend.Status as ShapeExtend.Status
import OpenCascade.Inheritance (upcast, downcast)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, unless)
import System.IO (hPutStrLn, stderr)
import Waterfall.Internal.Finalizers (toAcquire, fromAcquire)
import Data.Acquire
import Foreign.Ptr (Ptr)

-- | Write a `Solid` to a (binary) STL file at a given path
--
-- Because BRep representations of objects can store arbitrary precision curves,
-- but STL files store triangulated surfaces, 
-- this function takes a "deflection" argument used to discretize curves.
--
-- The deflection is the maximum allowable distance between a curve and the generated triangulation.
writeSTL :: Double -> FilePath -> Solid -> IO ()
writeSTL linDeflection filepath (Solid ptr) = (`withAcquire` pure) $ do
    s <- toAcquire ptr
    mesh <- BRepMesh.IncrementalMesh.fromShapeAndLinDeflection s linDeflection
    liftIO $ BRepMesh.IncrementalMesh.perform mesh
    writer <- StlWriter.new
    liftIO $ do
            StlWriter.setAsciiMode writer False
            res <- StlWriter.write writer s filepath
            unless res (hPutStrLn stderr ("failed to write " <> filepath))
    return ()

-- | Write a `Solid` to a STEP file at a given path
--
-- STEP files can be imported by [FreeCAD](https://www.freecad.org/)
writeSTEP :: FilePath -> Solid -> IO ()
writeSTEP filepath (Solid ptr) = (`withAcquire` pure) $ do
    s <- toAcquire ptr
    writer <- StepWriter.new
    _ <- liftIO $ StepWriter.transfer writer s StepModelType.Asls True
    void . liftIO $ StepWriter.write writer filepath

writeGLTFOrGLB :: Bool -> Double -> FilePath -> Solid -> IO ()
writeGLTFOrGLB binary linDeflection filepath (Solid ptr) = (`withAcquire` pure) $ do
    s <- toAcquire ptr
    mesh <- BRepMesh.IncrementalMesh.fromShapeAndLinDeflection s linDeflection
    liftIO $ BRepMesh.IncrementalMesh.perform mesh
    doc <- TDocStd.Document.fromStorageFormat ""
    mainLabel <- TDocStd.Document.main doc
    shapeTool <- XCafDoc.DocumentTool.shapeTool mainLabel
    _ <- XCafDoc.ShapeTool.addShape shapeTool s True True
    meta <- TColStd.IndexedDataMapOfStringString.new
    progress <- Message.ProgressRange.new
    writer <- RWGltf.CafWriter.new filepath binary
    liftIO $ RWGltf.CafWriter.perform writer doc meta progress
    return ()

-- | Write a `Solid` to a glTF file at a given path
--
-- glTF, or Graphics Library Transmission Format is a JSON based format 
-- used for three-dimensional scenes and models
writeGLTF :: Double -> FilePath -> Solid -> IO ()
writeGLTF = writeGLTFOrGLB False

-- | Write a `Solid` to a glb file at a given path
--
-- glb is the binary variant of the glTF file format
writeGLB :: Double -> FilePath -> Solid -> IO ()
writeGLB = writeGLTFOrGLB True

data ReadError = FileReadError | NonManifoldError deriving (Eq, Show)

makeMeshSolid :: Ptr TopoDS.Shape -> Acquire (Either ReadError (Ptr TopoDS.Shape))
makeMeshSolid s = do 
    shapeFix <- ShapeFix.Solid.new
    maybeShell <- liftIO $ downcast s
    case maybeShell of 
        Nothing -> pure . Left $ FileReadError
        Just shell -> do 
            solid <- upcast <$> ShapeFix.Solid.solidFromShell shapeFix shell
            open <- fmap not . liftIO $ TopoDS.Shape.closed s
            failed <- liftIO $ ShapeFix.Solid.status shapeFix ShapeExtend.Status.FAIL
            if failed || open
                then return . Left $ NonManifoldError
                else return . Right $ solid


-- | Read a `Solid` from an STL file at a given path
readSTL :: FilePath -> IO (Either ReadError Solid)
readSTL filepath = (fmap (fmap Solid)) . fromAcquire $ do
    shape <- TopoDS.Shape.new
    reader <- StlReader.new
    res <- liftIO $ StlReader.read reader shape filepath
    if res 
        then makeMeshSolid shape
        else return $ Left FileReadError