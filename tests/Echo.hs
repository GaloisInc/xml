import Text.XML.Light

main = do
        s <- readFile "rss.xml"
        case parseXMLDoc s of
          Nothing  -> error "Failed to parse xml"
          Just doc -> let v1 = showTopElement doc
                          Just doc2 = parseXMLDoc v1
                          v2 = showTopElement doc2
                      in print (v1 == v2)

