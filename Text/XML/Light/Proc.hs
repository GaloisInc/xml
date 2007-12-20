--------------------------------------------------------------------
-- |
-- Module    : 
-- Copyright : (c) Galois, Inc. 2007
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------


module Text.XML.Light.Proc where

import Text.XML.Light.Types

import Data.Maybe(listToMaybe,fromMaybe)
import Data.List(find)

-- | Get a list of the children for an element.  We return the
-- empty list for elements that do not support children.
elChildren         :: Element -> [Content]
elChildren e        = fromMaybe [] (elContent e)

-- | Get the text value of an XML element.  This function
-- ignores non-text elements, and concatenates all text elements.
strContent         :: Element -> String
strContent e        = concatMap cdData $ onlyText $ elChildren e

-- | Select only the elements from a list of XML content.
onlyElems          :: [Content] -> [Element]
onlyElems xs        = [ x | Elem x <- xs ]

-- | Select only the text from a list of XML content.
onlyText           :: [Content] -> [CData]
onlyText xs         = [ x | Text x <- xs ]

-- | Find all immediate children with the given name.
findChildren       :: QName -> Element -> [Element]
findChildren q e    = filter ((q ==) . elName) (onlyElems (elChildren e))

-- | Find an immediate child with the given name.
findChild          :: QName -> Element -> Maybe Element
findChild q e       = listToMaybe (findChildren q e)

-- | Find the left-most occurance of an element.
findElement        :: QName -> Element -> Maybe Element
findElement q e     = listToMaybe (findElements q e)

-- | Find all non-nested occurances of an element.
-- (i.e., once we have found an element, we do not search
-- for more occurances among the element's children).
findElements       :: QName -> Element -> [Element]
findElements qn e
 | elName e == qn = [e]
 | otherwise      = concatMap (findElements qn)
                  $ onlyElems $ elChildren e

-- | Lookup the value of an attribute.
findAttr           :: QName -> Element -> Maybe String
findAttr x e        = attrVal `fmap` find ((x ==) . attrKey) (elAttribs e)

