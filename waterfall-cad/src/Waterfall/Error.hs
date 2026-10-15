module Waterfall.Error
( WaterfallError (..)
) where

import Data.Data (Typeable)
import OpenCascade.Internal.Exception (OpenCascadeException)

-- | PLACEHOLDER
newtype WaterfallError = WaterfallError 
    { rawOpenCascadeError :: OpenCascadeException
    } deriving (Eq, Ord, Typeable, Show)