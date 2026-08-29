module OpenCascade.Graphic3D.AlphaMode
( AlphaMode (..)
) where

-- Should match the values in Graphic3D_AlphaMode.hxx
-- this can't derive Enum, because Graphic3d_AlphaMode_BlendAuto is defined as -1
data AlphaMode = BlendAuto | Opaque | Mask | Blend | MaskBlend deriving (Show, Eq)

instance Enum AlphaMode where
    fromEnum BlendAuto = -1
    fromEnum Opaque = 0
    fromEnum Mask = 1
    fromEnum Blend = 2
    fromEnum MaskBlend = 3
    toEnum (-1) = BlendAuto
    toEnum 0 = Opaque
    toEnum 1 = Mask
    toEnum 2 = Blend
    toEnum 3 = MaskBlend
    toEnum _ = error "toEnum: out of range for Graphic3D.AlphaMode"
