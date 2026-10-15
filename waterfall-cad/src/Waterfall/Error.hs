module Waterfall.Error
( WaterfallError (..)
) where

import Data.Data (Typeable)
import OpenCascade.Internal.Exception (OpenCascadeException)

-- | Error type, wrapping an `OpenCascadeException` thrown from the kernel
newtype WaterfallError = WaterfallError 
    { rawOpenCascadeError :: OpenCascadeException
    } deriving (Eq, Ord, Typeable, Show)