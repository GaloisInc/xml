module XML.Proc where

import XML.Types
import Data.Maybe(listToMaybe,fromMaybe)

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


