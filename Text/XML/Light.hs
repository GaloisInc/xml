--------------------------------------------------------------------
-- |
-- Module    : Text.XML.Light
-- Copyright : (c) Galois, Inc. 2007
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
-- Stability : provisional
-- Portability: portability
--
-- A lightweight XML parsing, filtering and generating library.
--

module Text.XML.Light (
    module Text.XML.Light,
    module X
  ) where

import Text.XML.Light.Types     as X
import Text.XML.Light.Proc      as X
import Text.XML.Light.Input     as X
import Text.XML.Light.Output    as X

-- | Add an attribute to an element.
add_attr :: Attr -> Element -> Element
add_attr a e = add_attrs [a] e

-- | Add some attributes to an element.
add_attrs :: [Attr] -> Element -> Element
add_attrs as e = e { elAttribs = as ++ elAttribs e }

-- | Create an unqualified name.
unqual :: String -> QName
unqual x = blank_name { qName = x }

-- | Create a node with a single text node as a child.
leaf :: QName -> String -> Element
leaf x y  = blank_element { elName = x
                          , elContent = Just [Text blank_cdata { cdData = y }]
                          }

-- | Create a node whose children are all elements.
-- Uses empy elements if there are no children.
node :: QName -> [Element] -> Element
node x ys = blank_element { elName = x
                          , elContent = case ys of
                                          [] -> Nothing
                                          _  -> Just (map Elem ys)
                          }

