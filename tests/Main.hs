import XML

main = do
	s <- readFile "rss.xml"
	case parseXMLDoc s of
		Nothing -> error "Failed to parse xml"
		Just doc  -> 
			let _:_:elem:_ = findElements (unqual "description") $ doc
		            Just [Text title_content] = elContent elem
			    title_text = cdData title_content
			in mapM_ putStrLn $ lines title_text

