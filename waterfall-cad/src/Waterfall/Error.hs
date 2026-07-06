module Waterfall.Error
( WaterfallError (..)
) where

import Data.Data (Typeable)
import OpenCascade.Internal.Exception (OpenCascadeException)

newtype WaterfallError = WaterfallError 
    { rawOpenCascadeError :: OpenCascadeException
    } deriving (Eq, Ord, Typeable, Show)