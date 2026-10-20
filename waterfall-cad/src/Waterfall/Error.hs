module Waterfall.Error
( WaterfallError (..)
) where

import Data.Data (Typeable)
import OpenCascade.Internal.Exception (OpenCascadeException (..))
import Control.DeepSeq(NFData (rnf))

-- | Error type, wrapping an `OpenCascadeException` thrown from the kernel
newtype WaterfallError = WaterfallError 
    { rawOpenCascadeError :: OpenCascadeException
    } deriving (Eq, Ord, Typeable, Show)

instance NFData WaterfallError where 
    rnf (WaterfallError e) = case e of
        OpenCascadeStandardFailure a b c -> rnf a `seq` rnf b `seq` rnf c
        OpenCascadeStdException s -> rnf s
        OpenCascadeOtherException -> ()