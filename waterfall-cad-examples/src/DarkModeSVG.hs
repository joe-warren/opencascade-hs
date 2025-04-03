{-# LANGUAGE QuasiQuotes #-}
module DarkModeSVG
( writeDarkModeSVG
) where

import qualified Waterfall.SVG
import qualified Waterfall
import qualified Graphics.Svg as Svg
import qualified Text.XML.Light.Types as XML
import qualified Text.XML.Light.Proc as XML.Proc
import qualified Text.XML.Light.Cursor as XML.Cursor
import qualified Text.XML.Light.Output as XML.Output
import Text.RawString.QQ
import Data.Function ((&))
import Data.Foldable (toList)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Monad (join)
styles :: String
styles = [r|
.edge {
  fill: None;
}
.edge.visible {
  stroke: #000000;
}
.edge.hidden {
  stroke: #C8C8FF;
}
@media (prefers-color-scheme: dark) {
    .edge.visible {
      stroke: #FFFFFF;
    }
    .edge.hidden {
      stroke: #A00000;
    }
}
|]

writeDarkModeSVG ::FilePath -> Waterfall.Diagram -> IO ()
writeDarkModeSVG path diagram =
    let svgAsXML = 
            diagram 
                & Waterfall.SVG.diagramToSvg
                & Svg.xmlOfDocument
        nameIsStyle (XML.Elem e) = (== "style") . XML.qName . XML.elName $ e
        nameIsStyle _ = False
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
            & XML.Output.ppTopElement
            & writeFile path

