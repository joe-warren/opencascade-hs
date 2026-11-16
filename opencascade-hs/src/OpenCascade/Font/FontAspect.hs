module OpenCascade.Font.FontAspect
( FontAspect (..)
) where

data FontAspect = Undefined | Regular | Bold | Italic | BoldItalic deriving (Eq, Ord, Show, Bounded)

-- this can't derive Enum, because FontAspect_Undefined is defined as -1
instance Enum FontAspect where
    fromEnum Undefined = -1
    fromEnum Regular = 0
    fromEnum Bold = 1
    fromEnum Italic = 2
    fromEnum BoldItalic = 3
    toEnum (-1) = Undefined
    toEnum 0 = Regular
    toEnum 1 = Bold
    toEnum 2 = Italic
    toEnum 3 = BoldItalic
    toEnum _ = error "toEnum: out of range for FontAspect"
    enumFrom x = enumFromTo x maxBound
    enumFromThen x y = 
        enumFromThenTo x y (
            if fromEnum y >= fromEnum x 
                then maxBound 
                else minBound
            )