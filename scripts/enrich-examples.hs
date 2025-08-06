#!/usr/bin/env stack
{- stack script --resolver lts-23.16
    --package xml
    --package bytestring
    --package filemanip
-}

import qualified Text.XML.Light as XML
import qualified Text.XML.Light.Cursor as C
import qualified Data.ByteString as BS
import Control.Arrow (first)
import Data.List (isSuffixOf)
import Control.Monad (when, void)
import System.FilePath.Find (find, (~~?), (==?), extension, filePath, fileName)

isMatchingImage :: C.Cursor -> Bool
isMatchingImage c = 
    case C.current c of 
        XML.Elem e -> let isImg = (XML.qName . XML.elName $ e) == "img"
                          src = XML.findAttrBy ((== "src") . XML.qName) e
                          isGLB = maybe False (isSuffixOf ".glb") src
                        in isImg && isGLB
        _ -> False


modifyImage :: XML.Content -> XML.Content
modifyImage (XML.Elem e) = 
    let src = maybe "" id $ XML.findAttrBy ((== "src") . XML.qName) e
        in XML.Elem . XML.node (XML.unqual "model-viewer") . fmap (uncurry XML.Attr . first XML.unqual) $
                [ ("src", src)
                , ("camera-controls", "")
                , ("touch-action", "pan-y")
                , ("auto-rotate", "")
                , ("rotation-per-second", "45deg")
                , ("shadow-intensity", "1")
                , ("style", "width: 100%; height: 600px; border: 1px dashed #5E5184;")
                ]
modifyImage x = x


isHead :: C.Cursor -> Bool
isHead c = 
    case C.current c of 
        XML.Elem e -> (XML.qName . XML.elName $ e) == "head"
        _ -> False


scriptTag :: XML.Content
scriptTag = XML.Elem $ XML.node (XML.unqual "script") 
                [ XML.Attr (XML.unqual "src") "https://ajax.googleapis.com/ajax/libs/model-viewer/4.0.0/model-viewer.min.js"
                , XML.Attr (XML.unqual "type") "module"
                ]

addScript :: C.Cursor -> C.Cursor
addScript c = case C.findRec isHead c >>= C.firstChild of 
    Nothing -> c
    Just c' -> C.root . C.insertRight scriptTag $ c'

modifyFile :: XML.Element -> (XML.Element, Bool)
modifyFile e = 
    let go (c, modified) = 
            case C.findRec isMatchingImage c of
                Nothing -> (c, modified)
                Just c' -> go (C.root . C.modifyContent modifyImage $ c', True)
        toElement (XML.Elem e) = e
        toElement _ = error "expected root to be element"
    in first (toElement . C.current . addScript) $ go (C.fromElement e, False)

processFile :: FilePath -> IO ()
processFile path = do
    bs <- BS.readFile path
    let Just doc = XML.parseXMLDoc bs
        (doc', modified) = modifyFile doc
        config = 
            XML.useShortEmptyTags ((/= "script") . XML.qName)
                $ XML.defaultConfigPP
    print (path, modified)
    when modified $ writeFile path (XML.ppcTopElement config doc')

-- (filePath ~~? "waterfall-cad-examples-*-docs")
main :: IO ()
main = do
    void $ traverse processFile 
        =<< foldMap (find (pure True) (extension ==? ".html"))
        =<< find (fileName ==? ".") (fileName ~~? "waterfall-cad-examples-*-docs") "."