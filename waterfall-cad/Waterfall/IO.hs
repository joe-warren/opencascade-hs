module Waterfall.IO
(  writeSTL
, writeSTEP
) where 

import Waterfall.Internal.Solid (Solid(..))
import qualified OpenCascade.BRepMesh.IncrementalMesh as BRepMesh.IncrementalMesh
import qualified OpenCascade.StlAPI.Writer as StlWriter
import qualified OpenCascade.STEPControl.Writer as StepWriter
import qualified OpenCascade.STEPControl.StepModelType as StepModelType
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Data.Acquire

writeSTL :: Double -> FilePath -> Solid -> IO ()
writeSTL linDeflection filepath (Solid runSolid) = (`withAcquire` pure) $ do
    s <- runSolid 
    mesh <- BRepMesh.IncrementalMesh.fromShapeAndLinDeflection s linDeflection
    liftIO $ BRepMesh.IncrementalMesh.perform mesh
    writer <- StlWriter.new
    liftIO $ StlWriter.setAsciiMode writer False
    liftIO $ StlWriter.write writer s filepath
    return ()

writeSTEP :: FilePath -> Solid -> IO ()
writeSTEP filepath (Solid runSolid) = (`withAcquire` pure) $ do
    s <- runSolid
    writer <- StepWriter.new
    _ <- liftIO $ StepWriter.transfer writer s StepModelType.Asls True
    void . liftIO $ StepWriter.write writer filepath

