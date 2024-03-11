module Waterfall.IO
(  writeSTL
, writeSTEP
, writeGLTF
, writeGLB
) where 

import Waterfall.Internal.Solid (Solid(..))
import qualified OpenCascade.BRepMesh.IncrementalMesh as BRepMesh.IncrementalMesh
import qualified OpenCascade.StlAPI.Writer as StlWriter
import qualified OpenCascade.STEPControl.Writer as StepWriter
import qualified OpenCascade.STEPControl.StepModelType as StepModelType
import qualified OpenCascade.TDocStd.Document as TDocStd.Document
import qualified OpenCascade.Message.ProgressRange as Message.ProgressRange
import qualified OpenCascade.TColStd.IndexedDataMapOfStringString as TColStd.IndexedDataMapOfStringString
import qualified OpenCascade.RWGltf.CafWriter as RWGltf.CafWriter
import qualified OpenCascade.XCAFDoc.DocumentTool as XCafDoc.DocumentTool
import qualified OpenCascade.XCAFDoc.ShapeTool as XCafDoc.ShapeTool
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, unless)
import System.IO (hPutStrLn, stderr)
import Waterfall.Internal.Finalizers (toAcquire)
import Data.Acquire

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