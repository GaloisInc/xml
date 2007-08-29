module XML.Types where

data Content  = Elem Element
              | Text CData
                deriving Show

data Element  = Element {
                  elName      :: QName,
                  elAttribs   :: [Attr],
                  elContent   :: Maybe [Content]
                } deriving Show

data Attr     = Attr {
                  attrKey :: QName,
                  attrVal :: String
                } deriving (Eq,Show)

data CData    = CData {
                  cdVerbatim  :: Bool,
                  cdData      :: String
                } deriving Show

data QName    = QName {
                  qName   :: String,
                  qURI    :: Maybe String,
                  qPrefix :: Maybe String
                } deriving Show


instance Eq QName where
  q1 == q2  = compare q1 q2 == EQ

instance Ord QName where
  compare q1 q2 =
    case compare (qName q1) (qName q2) of
      EQ  -> case (qURI q1, qURI q2) of
               (Nothing,Nothing) -> compare (qPrefix q1) (qPrefix q2)
               (u1,u2)           -> compare u1 u2
      x   -> x

unqual :: String -> QName
unqual x = QName { qName = x, qURI = Nothing, qPrefix = Nothing }

