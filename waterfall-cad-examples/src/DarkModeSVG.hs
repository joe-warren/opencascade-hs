{-# LANGUAGE OverloadedStrings #-}
module DarkModeSVG
( darkModeSVG
, writeDarkModeSVG
) where

import qualified Waterfall.SVG
import qualified Waterfall
import qualified Graphics.Svg as Svg
import qualified Graphics.Svg.CssTypes as Svg.Css
import qualified Text.XML.Light.Types as XML
import qualified Text.XML.Light.Proc as XML.Proc
import qualified Text.XML.Light.Cursor as XML.Cursor
import qualified Text.XML.Light.Output as XML.Output
import Data.Function ((&))
import Data.Foldable (toList)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Monad (join)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Graphics.Svg.CssTypes as Svg.Css
import qualified Graphics.Svg.CssTypes as Svg

combineStyles :: [Svg.Css.CssRule] -> [Svg.Css.CssRule] -> String
combineStyles lightModeRules darkModeRules = TL.unpack . TextBuilder.toLazyText $
  ( foldMap Svg.Css.tserialize lightModeRules 
    <> "\n @media (prefers-color-scheme: dark) {\n"
    <> foldMap Svg.Css.tserialize darkModeRules
    <> "\n}"
  )
darkModeSVG :: Waterfall.Diagram -> XML.Element
darkModeSVG diagram =
    let svgAsXML =
            diagram 
              & Waterfall.SVG.diagramToSvg
              & Svg.xmlOfDocument 
        nameIsStyle (XML.Elem e) = (== "style") . XML.qName . XML.elName $ e
        nameIsStyle _ = False
        hasColour = not . null . Waterfall.getDiagramFills $ diagram

        lightModeStyle = Waterfall.SVG.LineStyle
            { Waterfall.SVG.visibleLineColour = Waterfall.black
            , Waterfall.SVG.hiddenLineColour = 
                if hasColour
                  then Waterfall.black
                  else Waterfall.colourFromRGB24 0xC8C8FF
            , Waterfall.SVG.hiddenLineOpacity = 
                if hasColour 
                  then 0.2
                  else 1
            }
        darkModeStyle = Waterfall.SVG.LineStyle
            { Waterfall.SVG.visibleLineColour = Waterfall.white
            , Waterfall.SVG.hiddenLineColour = 
                if hasColour
                  then Waterfall.white
                  else Waterfall.colourFromRGB24 0xA00000
            , Waterfall.SVG.hiddenLineOpacity = 
                if hasColour 
                  then 0.2
                  else 1
            }
        styles = combineStyles (Waterfall.SVG.lineStyleCss lightModeStyle) (Waterfall.SVG.lineStyleCss darkModeStyle)
    in svgAsXML
            & XML.Cursor.fromElement
            & XML.Cursor.findChild (nameIsStyle . XML.Cursor.current)
            & fmap XML.Cursor.firstChild 
            & join
            & fmap (XML.Cursor.setContent (XML.Text (XML.CData XML.CDataText styles Nothing)))
            & fmap (XML.Cursor.root)
            & fmap XML.Cursor.current
            & toList
            & XML.Proc.onlyElems
            & listToMaybe
            & fromMaybe svgAsXML

writeDarkModeSVG :: FilePath -> Waterfall.Diagram -> IO ()
writeDarkModeSVG path diagram =
  diagram 
    & darkModeSVG
    & XML.Output.ppTopElement
    & writeFile path

