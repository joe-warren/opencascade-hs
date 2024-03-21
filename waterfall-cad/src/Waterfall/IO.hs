module Waterfall.IO
( ReadError (..)
, writeSTL
, writeSTEP
, writeGLTF
, writeGLB
, readSTL
, readSTEP
, readGLTF
, readGLB
) where 

import Waterfall.Internal.Solid (Solid(..), debug)
import qualified Waterfall.Internal.Remesh as Remesh
import qualified OpenCascade.BRepMesh.IncrementalMesh as BRepMesh.IncrementalMesh
import qualified OpenCascade.StlAPI.Writer as StlWriter
import qualified OpenCascade.StlAPI.Reader as StlReader
import qualified OpenCascade.STEPControl.Writer as StepWriter
import qualified OpenCascade.STEPControl.StepModelType as StepModelType
import qualified OpenCascade.STEPControl.Reader as STEPReader
import qualified OpenCascade.XSControl.Reader as XSControl.Reader
import qualified OpenCascade.IFSelect.ReturnStatus as IFSelect.ReturnStatus
import qualified OpenCascade.TDocStd.Document as TDocStd.Document
import qualified OpenCascade.Message.ProgressRange as Message.ProgressRange
import qualified OpenCascade.TColStd.IndexedDataMapOfStringString as TColStd.IndexedDataMapOfStringString
import qualified OpenCascade.RWGltf.CafWriter as RWGltf.CafWriter
import qualified OpenCascade.RWGltf.CafReader as RWGltf.CafReader
import qualified OpenCascade.RWMesh.CafReader as RWMesh.CafReader
import qualified OpenCascade.XCAFDoc.DocumentTool as XCafDoc.DocumentTool
import qualified OpenCascade.XCAFDoc.ShapeTool as XCafDoc.ShapeTool
import qualified OpenCascade.TopoDS.Types as TopoDS
import qualified OpenCascade.TopoDS.Shape as TopoDS.Shape
import qualified OpenCascade.TopoDS.Solid as TopoDS.Solid
import qualified OpenCascade.TopoDS.Shell as TopoDS.Shell
import qualified OpenCascade.TopoDS.Builder as TopoDS.Builder
import qualified OpenCascade.ShapeFix.Solid as ShapeFix.Solid
import qualified OpenCascade.ShapeExtend.Status as ShapeExtend.Status
import qualified OpenCascade.TopExp.Explorer as TopExp.Explorer
import qualified OpenCascade.TopAbs.ShapeEnum as ShapeEnum
import qualified OpenCascade.BRepBuilderAPI.MakeSolid as MakeSolid
import qualified OpenCascade.BRepBuilderAPI.Copy as BRepBuilderAPI.Copy
import qualified OpenCascade.BRepBuilderAPI.Sewing as BRepBuilderAPI.Sewing
import OpenCascade.Inheritance (upcast, downcast, unsafeDowncast)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (void, unless, when)
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
--
-- Because BRep representations of objects can store arbitrary precision curves,
-- but glTF files store triangulated surfaces, 
-- this function takes a "deflection" argument used to discretize curves.
--
-- The deflection is the maximum allowable distance between a curve and the generated triangulation.
writeGLTF :: Double -> FilePath -> Solid -> IO ()
writeGLTF = writeGLTFOrGLB False

-- | Write a `Solid` to a glb file at a given path
--
-- glb is the binary variant of the glTF file format
--
-- Because BRep representations of objects can store arbitrary precision curves,
-- but glTF files store triangulated surfaces, 
-- this function takes a "deflection" argument used to discretize curves.
--
-- The deflection is the maximum allowable distance between a curve and the generated triangulation.
writeGLB :: Double -> FilePath -> Solid -> IO ()
writeGLB = writeGLTFOrGLB True

data ReadError = FileReadError | NonManifoldError deriving (Eq, Show)

checkManifold :: MonadIO m => Ptr TopoDS.Shape -> m (Either ReadError (Ptr TopoDS.Shape))
checkManifold shape = do
    isNull <- liftIO . TopoDS.Shape.isNull $ shape
    if isNull 
        then return . Left $ FileReadError
        else do
            closed <- liftIO . TopoDS.Shape.closed $ shape
            return $ if closed 
                then Right $ shape
                else Left NonManifoldError

checkNonNull:: MonadIO m => Ptr TopoDS.Shape -> m (Either ReadError (Ptr TopoDS.Shape))
checkNonNull shape = do
    isNull <- liftIO . TopoDS.Shape.isNull $ shape
    return $ if isNull 
        then Left FileReadError
        else Right shape

makeMeshSolid :: Ptr TopoDS.Shape -> Acquire (Either ReadError (Ptr TopoDS.Shape))
makeMeshSolid s = do 
    shapeFix <- ShapeFix.Solid.new
    liftIO (print =<< TopoDS.Shape.shapeType s )
    maybeSolid <- liftIO $ downcast s :: Acquire (Maybe (Ptr TopoDS.Solid))
    maybeShell <- liftIO $ downcast s
    case (maybeSolid, maybeShell) of 
        (Nothing, Nothing) -> pure . Left $ FileReadError
        (Just _solid, _) -> return . Right $ s
        (_, Just shell) -> do 
            solid <- upcast <$> ShapeFix.Solid.solidFromShell shapeFix shell
            failed <- liftIO $ ShapeFix.Solid.status shapeFix ShapeExtend.Status.FAIL
            if failed
                then return . Left $ NonManifoldError
                else return . Right $ solid


-- | Read a `Solid` from an STL file at a given path
readSTL :: FilePath -> IO (Either ReadError Solid)
readSTL filepath = (fmap (fmap Solid)) . fromAcquire $ do
    shape <- TopoDS.Shape.new
    reader <- StlReader.new
    res <- liftIO $ StlReader.read reader shape filepath
    if res 
        then buildSolid shape
        else return $ Left FileReadError

-- | Read a `Solid` from a STEP file at a given path
--
-- This does far less validation on the returned data than it should
readSTEP :: FilePath -> IO (Either ReadError Solid)
readSTEP filepath = (fmap (fmap Solid)) . fromAcquire $ do
    reader <- STEPReader.new
    status <- liftIO $ XSControl.Reader.readFile (upcast reader) filepath
    _ <- liftIO $ XSControl.Reader.transferRoots (upcast reader)
    shape <- XSControl.Reader.oneShape (upcast reader)
    if status == IFSelect.ReturnStatus.Done
        then checkNonNull shape
        else return . Left $ FileReadError


buildSolid :: Ptr TopoDS.Shape -> Acquire (Either ReadError (Ptr TopoDS.Shape))
buildSolid s = do
    let linDeflection = 0.01
    mesh <- BRepMesh.IncrementalMesh.fromShapeAndLinDeflection s linDeflection
    liftIO $ BRepMesh.IncrementalMesh.perform mesh
    explorer <- TopExp.Explorer.new s ShapeEnum.Face
    tdsBuilder <- TopoDS.Builder.new
    makeSolid <- MakeSolid.new
    sewing <- BRepBuilderAPI.Sewing.new 1e-5 True True True False
    solid <- TopoDS.Solid.new
    liftIO $ BRepBuilderAPI.Sewing.load sewing (upcast solid)
    let go = do
            isMore <- liftIO $ TopExp.Explorer.more explorer
            when isMore $ do
                liftIO $ print "more"
                face <- liftIO $ TopExp.Explorer.value explorer
                face' <- BRepBuilderAPI.Copy.copy face True True
                liftIO . putStrLn . debug . Solid $ face
                shell <- TopoDS.Shell.new
                liftIO $ TopoDS.Builder.makeShell tdsBuilder shell
                liftIO $ TopoDS.Builder.add tdsBuilder (upcast shell) face'
                liftIO $ BRepBuilderAPI.Sewing.add sewing (upcast shell)
                liftIO $ MakeSolid.add makeSolid shell
                liftIO $ TopExp.Explorer.next explorer
                go
    go
    res <- MakeSolid.solid makeSolid
    liftIO . BRepBuilderAPI.Sewing.perform $ sewing
    res' <- BRepBuilderAPI.Sewing.sewedShape sewing

    return . Right . upcast $ res
    -- makeMeshSolid (upcast shell)
    
-- | Read a `Solid` from a GLTF file at a given path
--
-- This should support reading both the GLTF (json) and GLB (binary) formats
--
-- This does far less validation on the returned data than it should
readGLTF :: FilePath -> IO (Either ReadError Solid)
readGLTF  filepath = fmap (fmap Solid) . fromAcquire $ do
    reader <- RWGltf.CafReader.new 
    liftIO $ RWGltf.CafReader.setDoublePrecision reader True
    liftIO $ RWMesh.CafReader.setFileLengthUnit (upcast reader) 1
    doc <- TDocStd.Document.fromStorageFormat ""
    progress <- Message.ProgressRange.new
    _ <- liftIO $ RWMesh.CafReader.setDocument (upcast reader) doc
    res <- liftIO $ RWMesh.CafReader.perform (upcast reader) filepath progress
    if res 
        then fmap Right . Remesh.remesh =<< RWMesh.CafReader.singleShape (upcast reader)
        else return . Left $ FileReadError 

-- | Alias for `readGLTF`
--
-- This does far less validation on the returned data than it should
readGLB :: FilePath -> IO (Either ReadError Solid)
readGLB = readGLTF