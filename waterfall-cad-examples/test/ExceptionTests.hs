{-# LANGUAGE LambdaCase #-}
module ExceptionTests 
( exceptionTests
) where

import qualified Waterfall.Offset as Offset
import qualified Waterfall.Fillet as Fillet
import qualified Waterfall.Solids as Solids
import Waterfall.Error (WaterfallError (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import OpenCascade.Internal.Exception (OpenCascadeException (..), testThrow)
import OpenCascade.Std.RuntimeError as RuntimeError 
import OpenCascade.Inheritance (upcast)
import Control.Exception (catch, Exception)
import Control.Monad (when)
import System.IO.Temp (emptySystemTempFile)
import qualified Waterfall.IO as IO
import Waterfall.IO (WaterfallIOException(WaterfallIOException), WaterfallIOExceptionCause (FileError))
import Data.Acquire (with)
import Data.Either (fromLeft)


checkFailure :: Exception b => IO a -> (b -> Maybe String)  -> IO ()
checkFailure f check = do
    success <- catch (f >> pure True) handler
    when success $ assertFailure "Did not catch expected error"
    where
        handler e = 
            case check e of
                Nothing -> pure False 
                Just msg -> 
                    assertFailure msg

expectFailure :: (Exception b, Eq b) => IO a -> b -> IO ()
expectFailure f expected = 
    checkFailure f 
        (\e -> 
            if e == expected 
                then Nothing
                else Just $ "Caught unexpected error\n" 
                        <> "expected: " <> show expected
                        <> "\ngot: " <> show e
        )


writeTmpStl :: Solids.Solid -> IO ()
writeTmpStl s = do
    file <- emptySystemTempFile "solid.stl"
    IO.writeSTL 0.1 file s

exceptionTests :: TestTree
exceptionTests = testGroup "Exception Tests"
    [ -- It's a little weird that this fails at IO time, whereas _some_ operations will fail with an OpenCascade exception when the operation is performed
      testCase "Bad Offset" $ checkFailure (writeTmpStl $ Offset.offset (negate 1) Solids.centeredCube) 
            $ \case 
                WaterfallIOException FileError _ -> Nothing
                e -> Just $ "Expected WaterfallIOException\ngot: " <> show e
    , testCase "Bad Fillet" $ assertEqual "Bad Fillet" 
        (fromLeft (error "expected failure") $ Fillet.roundFilletEither 2 Solids.centeredCube)
        (WaterfallError $ OpenCascadeStandardFailure "BRep_API: command not done" "")
    , testCase "Throw Std Exception" $ with (RuntimeError.new "Some Exception Text") $ \re -> 
        expectFailure (testThrow (upcast re)) (OpenCascadeStdException "Some Exception Text")
    ]