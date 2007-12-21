{-# LANGUAGE FlexibleInstances #-}
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

-- | A smart element constructor which uses the type of its argument
-- to determine what sort of element to make.
class Node t where
  node :: t -> Element


instance Node (QName,[Attr],[Content]) where
  node (name,attrs,cont) = blank_element { elName     = name
                                         , elAttribs  = attrs
                                         , elContent  = cont
                                         }

instance Node (QName,[Attr])    where node (n,as) = node (n,as,[]::[Content])
instance Node (QName,Attr)            where node (n,a)  = node (n,[a])
instance Node QName                   where node n      = node (n,[]::[Attr])

instance Node (QName,[Content])       where node (n,cs) = node (n,[]::[Attr],cs)
instance Node (QName,Content)         where node (n,c)  = node (n,[c])
instance Node (QName,[Attr],Content)  where node (n,as,c) = node (n,as,[c])
instance Node (QName,Attr,Content)    where node (n,a,c)  = node (n,[a],[c])

instance Node (QName,[Attr],[Element]) where
  node (n,as,cs) = node (n,as,map Elem cs)

instance Node (QName,[Attr],Element)  where node (n,as,c) = node (n,as,[c])
instance Node (QName,Attr,Element)    where node (n,a,c)  = node (n,[a],c)
instance Node (QName,[Element])       where node (n,es) = node (n,[]::[Attr],es)
instance Node (QName,Element)         where node (n,e)  = node (n,[e])

instance Node (QName,[Attr],[CData]) where
  node (n,as,cs) = node (n,as,map Text cs)

instance Node (QName,[Attr],CData)  where node (n,as,c) = node (n,as,[c])
instance Node (QName,Attr,CData)    where node (n,a,c)  = node (n,[a],c)
instance Node (QName,[CData])       where node (n,es)   = node (n,[]::[Attr],es)
instance Node (QName,CData)         where node (n,e)    = node (n,[e])

instance Node (QName,[Attr],String) where
  node (n,as,t) = node (n,as,blank_cdata { cdData = t })

instance Node (QName,Attr,String)   where node (n,a,t)  = node (n,[a],t)
instance Node (QName,String)        where node (n,t)    = node (n,[]::[Attr],t)




