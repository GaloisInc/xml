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

nodes cur_info ps (TokStart p t as empty : ts) = (node : siblings, open, toks)
  where
  new_name  = annotName new_info t
  new_info  = foldr addNS cur_info as
  node      = Elem Element { elLine    = Just p,
                             elName    = new_name,
                             elAttribs = map (annotAttr new_info) as,
                             elContent = children }

  (children,(siblings,open,toks))
    | empty     = (Nothing, nodes cur_info ps ts)
    | otherwise = let (es1,qs1,ts1) = nodes new_info (new_name:ps) ts
                  in (Just es1,
                      case qs1 of
                        [] -> nodes cur_info ps ts1
                        _ : qs3 -> ([],qs3,ts1))

nodes ns ps (TokEnd p t : ts)   = let t1 = annotName ns t
                                in case break (t1 ==) ps of
                                  (as,_:_) -> ([],as,ts)
                                  -- Unknown closing tag. Insert as text.
                                  (_,[]) ->
                                    let (es,qs,ts1) = nodes ns ps ts
                                    in (Text CData {
                                               cdLine = Just p,
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

type LChar              = (Line,Char)
type LString            = [LChar]
data Token              = TokStart Line QName [Attr] Bool  -- is empty?
                        | TokEnd Line QName
                        | TokText CData
                          deriving Show

tokens             :: String -> [Token]
tokens = tokens' . linenumber 1

tokens' :: LString -> [Token]
tokens' ((_,'<') : c@(_,'!') : cs) = special c cs

tokens' ((_,'<') : cs)   = tag (dropSpace cs) -- we are being nice here
tokens' [] = []
tokens' cs@((l,_):_) = let (as,bs) = breakn ('<' ==) cs
                       in TokText CData { cdLine = Just l,
                                          cdVerbatim = False,
                                          cdData = decode as } : tokens' bs

special :: LChar -> LString -> [Token]
special _ ((_,'-') : (_,'-') : cs) = skip cs
  where skip ((_,'-') : (_,'-') : (_,'>') : ds) = tokens' ds
        skip (_ : ds) = skip ds
        skip [] = [] -- unterminated comment

special c ((_,'[') : (_,'C') : (_,'D') : (_,'A') : (_,'T') : (_,'A') : (_,'[')
         : cs) =
  let (xs,ts) = cdata cs
  in TokText CData { cdLine = Just (fst c), cdVerbatim = True, cdData = xs }
                                                                  : tokens' ts
  where cdata ((_,']') : (_,']') : (_,'>') : ds) = ([],ds)
        cdata ((_,d) : ds)  = let (xs,ys) = cdata ds in (d:xs,ys)
        cdata []        = ([],[])

special c cs = tag (c : cs) -- invalid specials are processed as tags


qualName           :: LString -> (QName,LString)
qualName xs         = let (as,bs) = breakn endName xs
                          (q,n)   = case break (':'==) as of
                                      (q1,_:n1) -> (Just q1, n1)
                                      _         -> (Nothing, as)
                      in (QName { qURI = Nothing, qPrefix = q, qName = n }, bs)
  where endName x = isSpace x || x == '=' || x == '>' || x == '/'





tag              :: LString -> [Token]
tag ((p,'/') : cs)    = let (n,ds) = qualName (dropSpace cs)
                        in TokEnd p n : case ds of
                                          (_,'>') : es -> tokens' es
                                          -- tag was not properly closed...
                                          _        -> tokens' ds
tag []            = []
tag cs            = let (n,ds)  = qualName cs
                        (as,b,ts) = attribs (dropSpace ds)
                    in TokStart (fst (head cs)) n as b : ts

attribs          :: LString -> ([Attr], Bool, [Token])
attribs cs        = case cs of
                      (_,'>') : ds -> ([], False, tokens' ds)

                      (_,'/') : ds -> ([], True, case ds of
                                              (_,'>') : es -> tokens' es
                                              -- insert missing >  ...
                                              _ -> tokens' ds)

                      (_,'?') : (_,'>') : ds -> ([], False, tokens' ds)

                      -- doc ended within a tag..
                      []       -> ([],False,[])

                      _        -> let (a,cs1) = attrib cs
                                      (as,b,ts) = attribs cs1
                                  in (a:as,b,ts)

attrib             :: LString -> (Attr,LString)
attrib cs           = let (ks,cs1)  = qualName cs
                          (vs,cs2)  = attr_val (dropSpace cs1)
                      in ((Attr ks (decode vs)),dropSpace cs2)

attr_val           :: LString -> (String,LString)
attr_val ((_,'=') : cs) = string (dropSpace cs)
attr_val cs         = ("",cs)


dropSpace :: LString -> LString
dropSpace = dropWhile (isSpace . snd)

-- | Match the value for an attribute.  For malformed XML we do
-- our best to guess the programmer's intention.
string             :: LString -> (String,LString)
string ((_,'"') : cs)   = break' ('"' ==) cs

-- Allow attributes to be enclosed between ' '.
string ((_,'\'') : cs)  = break' ('\'' ==) cs

-- Allow attributes that are not enclosed by anything.
string cs           = breakn eos cs
  where eos x = isSpace x || x == '>' || x == '/'


break' :: (a -> Bool) -> [(b,a)] -> ([a],[(b,a)])
break' p xs         = let (as,bs) = breakn p xs
                      in (as, case bs of
                                [] -> []
                                _ : cs -> cs)

breakn :: (a -> Bool) -> [(b,a)] -> ([a],[(b,a)])
breakn p l = (map snd as,bs) where (as,bs) = break (p . snd) l

decode :: String -> String
decode ('&' : 'l' : 't' : ';' : cs)             = '<'   : decode cs
decode ('&' : 'g' : 't' : ';' : cs)             = '>'   : decode cs
decode ('&' : 'a' : 'm' : 'p' : ';' : cs)       = '&'   : decode cs
decode ('&' : 'a' : 'p' : 'o' : 's' : ';' : cs) = '\''  : decode cs
decode ('&' : 'q' : 'u' : 'o' : 't' : ';' : cs) = '"'   : decode cs
decode ('&' : '#' : cs) = case char_num cs of
                            Just (c,ds) -> c : decode ds
                            _ -> '&' : '#' : decode cs  -- Invalid escape.
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

linenumber :: Line -> String -> LString
linenumber _ [] = []
linenumber n ('\n':s) = n' `seq` ((n,'\n'):linenumber n' s) where n' = n + 1
linenumber n (c:s)    = (n,c) : linenumber n s
