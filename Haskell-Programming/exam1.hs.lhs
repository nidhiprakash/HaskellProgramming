wordCount::String -> (Int,Int,Int)
wordCount inp =(l,w,c)
	where l = length(lines inp)
		  w = length(words inp)
		  c = length(inp)

main:: IO()
main = do inp <- getContents
		  print (wordCount inp)