import Text.XML.Light

pp = useShortEmptyTags (const False) defaultConfigPP

main = putStrLn $ ppcTopElement pp
                $ Element { elName = unqual "test"
                          , elAttribs = []
                          , elContent = []
                          , elLine = Nothing
                          }

