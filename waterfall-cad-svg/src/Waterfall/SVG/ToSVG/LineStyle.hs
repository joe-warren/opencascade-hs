{-|
Module: Waterfall.SVG.ToSVG.LineStyle

Specify how the `Waterfall.Path2D`s in a `Waterfall.Diagram.Diagram` are displayed when it's converted to SVG (see `Waterfall.SVG.ToSVG.diagramToSvgWithLineStyle`).

`DiagramStyle` values can be constructed from the @lineWith*@ and @diagramWith*@ functions and can be combined monoidally (with `<>`), e.g.

@
thickVisibleThinHiddenLines =
    diagramWithVisibleLine 
        ( lineWithColour (Colour 0 0 0) 
            <> lineWithOpacity 1.0
            <> lineWithWidth 3.0
        ) 
    <> diagramWithHiddenLine
        ( lineWithColour (Colour 0.3 0.3 1.0) 
            <> lineWithOpacity 0.2
            <> lineWithWidth 0.5
        )
@

Properties that are not set fall back to the values in `defaultDiagramStyle`
-}

{-# LANGUAGE DeriveGeneric, DerivingVia, InstanceSigs #-}
module Waterfall.SVG.ToSVG.LineStyle
( DiagramStyle
, LineStyle
, defaultDiagramStyle
-- * Constructors
, lineWithColour
, lineWithOpacity
, lineWithWidth
, diagramWithVisibleLine
, diagramWithHiddenLine
-- * Optics
, lineColour
, lineOpacity
, lineWidth
, diagramVisibleLine
, diagramHiddenLine
) where

import qualified Waterfall
import GHC.Generics (Generic, Generically (..))
import Control.DeepSeq (NFData)
import Data.Monoid (Last (..), Endo (..))
import Data.Maybe (catMaybes)
import Data.List (intersperse)
import Control.Lens (Lens', lens)

-- | How the lines in a diagram should appear
--
-- `DiagramStyle` values can be constructed from the @diagramWith*@ functions and can be combined monoidally (with `<>`).
data DiagramStyle = DiagramStyle
    { _visibleLine :: LineStyle
    , _hiddenLine :: LineStyle
    } deriving (Eq, Ord, Generic)
    deriving (Semigroup, Monoid) via (Generically DiagramStyle)

instance NFData DiagramStyle

instance Show DiagramStyle where 
  showsPrec :: Int -> DiagramStyle -> ShowS
  showsPrec d p = 
    let showTerm name term =
            showString name
            . showChar ' '
            . showsPrec 11 term
        showVisible = showTerm "diagramWithVisibleLine" (_visibleLine p)
        showHidden = showTerm "diagramWithHiddenLine" (_hiddenLine p)
    in case (_visibleLine p == mempty, _hiddenLine p == mempty) of
        (True, True) -> showString "mempty"
        (True, False) -> showParen (d > 10) showHidden
        (False, True) -> showParen (d > 10) showVisible
        (False, False) ->
            showParen (d > 6)
            ( showVisible
              . showString " <> "
              . showHidden
            )

-- | How a particular set of lines (visible or hidden) should appear
--
-- `LineStyle` values can be constructed from the @lineWith*@ functions and can be combined monoidally (with `<>`).
data LineStyle = LineStyle
    { _lineColour :: Last Waterfall.Colour
    , _lineOpacity :: Last Double
    , _lineWidth :: Last Double
    } deriving (Eq, Ord, Generic)
    deriving (Semigroup, Monoid) via (Generically LineStyle)

instance NFData LineStyle

instance Show LineStyle where
  showsPrec :: Int -> LineStyle -> ShowS
  showsPrec d p = 
    let term name field = 
            let fv v = 
                    showString name
                     . showChar ' '
                     . showsPrec 11 v 
            in fv <$> getLast (field p)
        terms = catMaybes 
            [ term "lineWithColour" _lineColour
            , term "lineWithOpacity" _lineOpacity
            , term "lineWithWidth" _lineWidth
            ]
    in case terms of
        [] -> showString "mempty"
        _ -> showParen (d > 6) . appEndo
            . foldMap Endo
            . intersperse (showString " <> ") 
            $ terms

-- | Create a `LineStyle` with a given `Waterfall.Colour`
lineWithColour :: Waterfall.Colour -> LineStyle
lineWithColour c = mempty {_lineColour = Last . Just $ c}

-- | The `Waterfall.Colour` used to draw a set of lines
--
-- Used to set the @stroke@ CSS property
lineColour :: Lens' LineStyle (Maybe Waterfall.Colour)
lineColour = lens
    (getLast . _lineColour)
    (\ls c -> ls {_lineColour = Last c})

-- | Create a `LineStyle` with a given opacity
--
-- Opacity is in the range [0, 1]
lineWithOpacity :: Double -> LineStyle
lineWithOpacity opacity = mempty {_lineOpacity = Last . Just $ opacity}

-- | The opacity used to draw a set of lines
--
-- Used to set the @stroke-opacity@ CSS property
lineOpacity :: Lens' LineStyle (Maybe Double)
lineOpacity = lens
    (getLast . _lineOpacity)
    (\ls c -> ls {_lineOpacity = Last c})

-- | Create a `LineStyle` with a given width
--
-- Width is in SVG user units
lineWithWidth :: Double -> LineStyle
lineWithWidth width = mempty {_lineWidth= Last . Just $ width}

-- | The width used to draw a set of lines
--
-- Used to set the @stroke-width@ CSS property
lineWidth :: Lens' LineStyle (Maybe Double)
lineWidth = lens
    (getLast . _lineWidth)
    (\ls c -> ls {_lineWidth = Last c})

-- | Create a `DiagramStyle` specifying how to draw visible lines
diagramWithVisibleLine :: LineStyle -> DiagramStyle
diagramWithVisibleLine lineStyle = mempty {_visibleLine = lineStyle}

-- | The visible line style for a `DiagramStyle`
diagramVisibleLine :: Lens' DiagramStyle LineStyle
diagramVisibleLine = lens _visibleLine (\ds ls -> ds {_visibleLine = ls})

-- | Create a `DiagramStyle` specifying how to draw hidden lines
diagramWithHiddenLine :: LineStyle -> DiagramStyle
diagramWithHiddenLine lineStyle = mempty {_hiddenLine = lineStyle}

-- | The hidden line style for a `DiagramStyle`
diagramHiddenLine :: Lens' DiagramStyle LineStyle
diagramHiddenLine = lens _hiddenLine (\ds ls -> ds {_hiddenLine = ls})

-- | Fallback values used for a property of a `DiagramStyle` which is unset
--
-- @
-- defaultDiagramStyle /= mempty
-- @
defaultDiagramStyle :: DiagramStyle
defaultDiagramStyle = DiagramStyle
    { _visibleLine = LineStyle 
        { _lineColour = Last . Just $ Waterfall.Colour 0 0 0
        , _lineOpacity = Last . Just $ 1
        , _lineWidth = Last . Just $ 1
        }
    , _hiddenLine = LineStyle 
        { _lineColour = Last . Just $ Waterfall.Colour 0 0 0
        , _lineOpacity = Last . Just $ 0.4
        , _lineWidth = Last . Just $ 1
        }
    }
