module XML.Input (parseXML,parseXMLDoc,tokens) where

import XML.Types
import XML.Proc
import XML.Output(tagEnd)

import Data.Char(isSpace)
import Data.List(isPrefixOf)
import Numeric(readHex)


parseXMLDoc  :: String -> Maybe Element
parseXMLDoc xs  = strip (parseXML xs)
  where strip cs = case onlyElems cs of
                    e : _
                      | "?xml" `isPrefixOf` qName (elName e)
                          -> strip (elChildren e)
                      | otherwise -> Just e
                    _ -> Nothing

parseXML   :: String -> [Content]
parseXML xs = parse $ tokens $ preprocess xs

parse      :: [Token] -> [Content]
parse []    = []
parse ts    = let (es,_,ts1) = nodes ([],Nothing) [] ts
              in es ++ parse ts1

-- Information about namespaces.
-- The first component is a map that associates prefixes to URIs,
-- the second is the URI for the default namespace, if one was provided.
type NSInfo = ([(String,String)],Maybe String)

nodes :: NSInfo -> [QName] -> [Token] -> ([Content], [QName], [Token])

nodes ns ps (TokText txt : ts) =
  let (es,qs,ts1) = nodes ns ps ts
      (more,es1)  = case es of
                      Text cd : es1'
                        | cdVerbatim cd == cdVerbatim txt -> (cdData cd,es1')
                      _                                   -> ([],es)

  in (Text txt { cdData = cdData txt ++ more } : es1, qs, ts1)

nodes cur_info ps (TokStart t as empty : ts) = (node : siblings, open, toks)
  where
  new_name  = annotName new_info t
  new_info  = foldr addNS cur_info as
  node      = Elem Element { elName    = new_name,
                             elAttribs = map (annotAttr new_info) as,
                             elContent = children }

  (children,(siblings,open,toks))
    | empty     = (Nothing, nodes cur_info ps ts)
    | otherwise = let (es1,qs1,ts1) = nodes new_info (new_name:ps) ts
                  in (Just es1,
                      case qs1 of
                        [] -> nodes cur_info ps ts1
                        _ : qs3 -> ([],qs3,ts1))

nodes ns ps (TokEnd t : ts)   = let t1 = annotName ns t
                                in case break (t1 ==) ps of
                                  (as,_:_) -> ([],as,ts)
                                  -- Unknown closing tag. Insert as text.
                                  (_,[]) ->
                                    let (es,qs,ts1) = nodes ns ps ts
                                    in (Text CData {
                                               cdVerbatim = False,
                                               cdData = tagEnd t ""
                                              } : es,qs, ts1)

nodes _ ps []                 = ([],ps,[])


annotName :: NSInfo -> QName -> QName
annotName (namespaces,def_ns) n =
  n { qURI = maybe def_ns (`lookup` namespaces) (qPrefix n) }

annotAttr :: NSInfo -> Attr -> Attr
annotAttr ns a@(Attr { attrKey = k}) =
  case (qPrefix k, qName k) of
    (Nothing,"xmlns") -> a
    _                 -> a { attrKey = annotName ns k }

addNS :: Attr -> NSInfo -> NSInfo
addNS (Attr { attrKey = key, attrVal = val }) (ns,def) =
  case (qPrefix key, qName key) of
    (Nothing,"xmlns") -> (ns, if null val then Nothing else Just val)
    (Just "xmlns", k) -> ((k, val) : ns, def)
    _                 -> (ns,def)


-- Lexer -----------------------------------------------------------------------

data Token              = TokStart QName [Attr] Bool  -- is empty?
                        | TokEnd QName
                        | TokText CData
                          deriving Show


tokens             :: String -> [Token]
tokens ('<' : '!' : cs) = special cs

tokens ('<' : cs)   = tag (dropWhile isSpace cs)
tokens []           = []
tokens cs           = let (as,bs) = break ('<' ==) cs
                      in TokText (CData False (decode as)) : tokens bs

special :: String -> [Token]
special ('-' : '-' : cs) = skip cs
  where skip ('-' : '-' : '>' : ds) = tokens ds
        skip (_ : ds) = skip ds
        skip [] = []

special ('[' : 'C' : 'D' : 'A' : 'T' : 'A' : '[' : cs) =
  let (xs,ts) = cdata cs
  in TokText (CData True xs) : tokens ts
  where cdata (']' : ']' : '>' : ds) = ([],ds)
        cdata (d : ds)  = let (xs,ys) = cdata ds in (d:xs,ys)
        cdata []        = ([],[])

special cs = tag ('!' : cs)


qualName           :: String -> (QName,String)
qualName xs         = let (as,bs) = break endName xs
                          (q,n)   = case break (':'==) as of
                                      (q1,_:n1) -> (Just q1, n1)
                                      _         -> (Nothing, as)
                      in (QName { qURI = Nothing, qPrefix = q, qName = n }, bs)
  where endName x = isSpace x || x == '=' || x == '>' || x == '/'





tag              :: String -> [Token]
tag ('/' : cs)    = let (n,ds) = qualName (dropWhile isSpace cs)
                    in TokEnd n : case ds of
                                    '>' : es -> tokens es
                                    -- tag was not properly closed...
                                    _        -> tokens ds
tag []            = []
tag cs            = let (n,ds)  = qualName cs
                        (as,b,ts) = attribs (dropWhile isSpace ds)
                    in TokStart n as b : ts

attribs          :: String -> ([Attr], Bool, [Token])
attribs cs        = case cs of
                      '>' : ds -> ([], False, tokens ds)

                      '/' : ds -> ([], True, case ds of
                                              '>' : es -> tokens es
                                              -- insert missing >  ...
                                              _ -> tokens ds)

                      '?' : '>' : ds -> ([], False, tokens ds)

                      -- doc ended within a tag..
                      []       -> ([],False,[])

                      _        -> let (a,cs1) = attrib cs 
                                      (as,b,ts) = attribs cs1
                                  in (a:as,b,ts)

attrib             :: String -> (Attr,String)
attrib cs           = let (ks,cs1)  = qualName cs
                          (vs,cs2)  = attr_val (dropWhile isSpace cs1)
                      in ((Attr ks (decode vs)),dropWhile isSpace cs2)

attr_val            :: String -> (String,String)
attr_val ('=' : cs) = string (dropWhile isSpace cs)
attr_val cs         = ("",cs)


-- | Match the value for an attribute.  For malformed XML we do
-- our best to guess the programmer's intention.
string             :: String -> (String,String)
string ('"' : cs)   = break' ('"' ==) cs

-- Allow attributes to be enclosed between ' '.
string ('\'' : cs)  = break' ('\'' ==) cs

-- Allow attributes that are not enclosed by anything.
string cs           = break eos cs
  where eos x = isSpace x || x == '>' || x == '/'


break' :: (a -> Bool) -> [a] -> ([a],[a])
break' p xs         = let (as,bs) = break p xs
                      in (as, case bs of
                                [] -> []
                                _ : cs -> cs)

decode :: String -> String
decode ('&' : 'l' : 't' : ';' : cs)             = '<'   : decode cs
decode ('&' : 'g' : 't' : ';' : cs)             = '>'   : decode cs
decode ('&' : 'a' : 'm' : 'p' : ';' : cs)       = '&'   : decode cs
decode ('&' : 'a' : 'p' : 'o' : 's' : ';' : cs) = '\''  : decode cs
decode ('&' : 'q' : 'u' : 'o' : 't' : ';' : cs) = '"'   : decode cs
decode ('&' : '#' : cs) = case char_num cs of
                            Just (c,ds) -> c : decode ds
                            _ -> '&' : '#' : decode cs
decode (c : cs)                                 = c     : decode cs
decode []                                       = []

char_num :: String -> Maybe (Char,String)
char_num cs = case cs of
                'x' : ds -> next (readHex ds)
                _        -> next (reads cs)
  where
  next [(n,';':ds)] = do c <- cvt_char n
                         return (c,ds)
  next _            = Nothing

  cvt_char :: Int -> Maybe Char
  cvt_char x
    | fromEnum (minBound :: Char) <= x && x <= fromEnum (maxBound::Char)
                = Just (toEnum x)
    | otherwise = Nothing


preprocess :: String -> String
preprocess ('\r' : '\n' : cs) = '\n' : preprocess cs
preprocess ('\r' : cs)        = '\n' : preprocess cs
preprocess (c : cs)           = c : preprocess cs
preprocess []                 = []

