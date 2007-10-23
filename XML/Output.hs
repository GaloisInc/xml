module XML.Output
  ( showTopElement, showContent, showElement, showCData, showQName, showAttr
  , ppTopElement, ppContent, ppElement
  , dumpContent, dumpElement
  , tagEnd
  ) where

import Data.Char
import XML.Types

xml_header :: String
xml_header = "<?xml version='1.0' ?>"

-- Pretty preting renders XML documents faithfully,
-- with the exception that whietspace may be added/removed
-- in non-verbatim character data.

ppTopElement       :: Element -> String
ppTopElement e      = unlines [xml_header,ppElement e]

ppElement          :: Element -> String
ppElement e         = ppElementS "" e ""

ppContent          :: Content -> String
ppContent x         = ppContentS "" x ""

ppContentS         :: String -> Content -> ShowS
ppContentS i x xs   = case x of
                        Elem e -> ppElementS i e xs
                        Text c -> ppCData i c xs

ppElementS         :: String -> Element -> ShowS
ppElementS i e xs   = i ++ (tagStart (elName e) (elAttribs e) $
  case elContent e of
    Nothing -> "/>" ++ xs
    Just [Text t] -> ">" ++ ppCData "" t (tagEnd (elName e) xs)
    Just cs -> ">\n" ++ foldr ppSub (i ++ tagEnd (elName e) xs) cs
      where ppSub e1 = ppContentS ("  " ++ i) e1 . showChar '\n'
  )

ppCData            :: String -> CData -> ShowS
ppCData i c xs      = i ++ if cdVerbatim c
                              then showCDataS c xs
                              else foldr cons xs (showCData c)

  where cons         :: Char -> String -> String
        cons '\n' ys  = "\n" ++ i ++ ys
        cons y ys     = y : ys



--------------------------------------------------------------------------------
dumpContent        :: Content -> String
dumpContent c       = dumpContentS 1 c ""

dumpElement        :: Element -> String
dumpElement c       = dumpElementS 1 c ""

-- | Show a tree view (adds white space).
-- The first argument is indentation.
dumpContentS       :: Int -> Content -> ShowS
dumpContentS n e xs = case e of
                        Elem e1 -> dumpElementS n e1 xs
                        Text c  -> dumpCData n c xs

dumpElementS       :: Int -> Element -> ShowS
dumpElementS n e xs = indent ++
  case elContent e of 
    Just cs -> tagStart (elName e) (elAttribs e) $    -- don't esc attrs?
              ">\n" ++ foldr shSub (indent ++ tagEnd (elName e) xs) cs
      where shSub e1 = dumpContentS (n+1) e1 . showChar '\n'
    Nothing -> tagStart (elName e) (elAttribs e) ("/>\n" ++ xs)
  where indent = indentElem n

dumpCData          :: Int -> CData -> ShowS
dumpCData n c xs    = indentElem n ++ foldr cons xs (cdData c)
  where cons '\n' ys  = "\n" ++ indent ++ ys
        cons y ys     = y : ys

        indent        = indentCData n

indentElem         :: Int -> String
indentElem 0        = ""
indentElem n        = concat (replicate (n-1) "| ") ++ "|-"

indentCData        :: Int -> String
indentCData 0       = ""
indentCData n       = concat (replicate (n-1) "| ") ++ "|."



--------------------------------------------------------------------------------
-- | Adds the <?xml?> header.
showTopElement     :: Element -> String
showTopElement c    = xml_header ++ showElement c

showContent            :: Content -> String
showContent c           = showContentS c ""

showElement        :: Element -> String
showElement c       = showElementS c ""

showCData          :: CData -> String
showCData c         = showCDataS c ""

-- | Good for transmition (no extra white space etc.) but less readable.
showContentS           :: Content -> ShowS
showContentS (Elem e)   = showElementS e
showContentS (Text cs)  = showCDataS cs

-- | Good for transmition (no extra white space etc.) but less readable.
showElementS       :: Element -> ShowS
showElementS (Element qn as cs) xs =
  tagStart qn as $ case cs of
                     Just ch -> '>' : foldr showContentS (tagEnd qn xs) ch
                     Nothing -> "/>" ++ xs

-- | Convert a text element to characters.
showCDataS         :: CData -> ShowS
showCDataS (CData isC str)
 | isC              = showString "<![CDATA[" . escCData str . showString "]]>"
 | otherwise        = escStr str


--------------------------------------------------------------------------------
escCData           :: String -> ShowS
escCData (']' : ']' : '>' : cs) = showString "]]]]><![CDATA[>" . escCData cs
escCData (c : cs)               = showChar c . escCData cs
escCData []                     = id

escChar            :: Char -> ShowS
escChar c = case c of
  '<'   -> showString "&lt;"
  '>'   -> showString "&gt;"
  '&'   -> showString "&amp;"
  '"'   -> showString "&quot;"
  '\''  -> showString "&apos;"
  -- XXX: Is this really wortherd?
  -- We could deal with these issues when we convert characters to bytes.
  _ | (oc <= 0x7f && isPrint c) || c == '\n' -> showChar c
    | otherwise -> showString "&#" . shows oc . showChar ';'
      where oc = ord c

escStr             :: String -> ShowS
escStr cs rs        = foldr escChar rs cs

tagEnd             :: QName -> ShowS
tagEnd qn rs        = '<':'/':showQName qn ++ '>':rs

tagStart           :: QName -> [Attr] -> ShowS
tagStart qn as rs   = '<':showQName qn ++ as_str ++ rs
 where as_str       = if null as then "" else ' ' : unwords (map showAttr as)

showAttr           :: Attr -> String
showAttr (Attr qn v) = showQName qn ++ '=' : '"' : escStr v "\""

showQName          :: QName -> String
showQName q         = pre ++ qName q
  where pre = case qPrefix q of
                Nothing -> ""
                Just p  -> p ++ ":"



