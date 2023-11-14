module Waterfall.IO
(  writeSTL
) where 

import qualified Waterfall.Solids as Solids
import qualified OpenCascade.BRepMesh.IncrementalMesh as BRepMesh.IncrementalMesh
import qualified OpenCascade.StlAPI.Writer as StlWriter
import qualified OpenCascade.Inheritance as Inheritance
import Control.Monad.IO.Class (liftIO)
import Data.Acquire

writeSTL :: Double -> FilePath -> Solids.Solid -> IO ()
writeSTL linDeflection filepath (Solids.Solid runSolid) = (`withAcquire` pure) $ do
    s <- runSolid 
    mesh <- BRepMesh.IncrementalMesh.fromShapeAndLinDeflection (Inheritance.upcast s) linDeflection
    liftIO $ BRepMesh.IncrementalMesh.perform mesh
    writer <- StlWriter.new
    liftIO $ StlWriter.setAsciiMode writer False
    liftIO $ StlWriter.write writer (Inheritance.upcast s) filepath
    return ()

