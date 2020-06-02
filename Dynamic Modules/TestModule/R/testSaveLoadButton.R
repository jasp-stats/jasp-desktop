testSaveLoadButton <- function(jaspResults, dataset, options)
{
	watts <-  "I mean here we are, and isn’t that odd? Of course it is odd, but what do you mean by odd?
	Well, it is what is different from even, and what is odd stands out. 
	What is even lies flat, but you cannot see the outstanding without the flat background. 
	
	Is the thing standing out? It’s odd. 
	
	Each one of you is odd: strange, unique, particular, different. 
	How do we know what we mean by that, except against the background of something even that is not differentiated, like space?"

	print('options$saveHere:')
	print( options$saveHere  )
	print('options$loadThis:')
	print( options$loadThis  )

	if(options$saveHere != "")
	{
		writeLines(watts, con = options$saveHere, sep = "\n", useBytes = FALSE)

		jaspResults[["savedHere"]] <- createJaspHtml(text=paste0("Saved a text to '", options$saveHere))
	}
	else
		jaspResults[["savedHere"]] <- createJaspHtml(text="No filename to save to was given")

	if(options$loadThis != "")
	{
		loadedThis <- readLines(con = options$loadThis)

		jaspResults[["loadedThis"]] <- createJaspHtml(text=paste0("Loaded file '", options$loadThis, "' which contains the text:\n'", paste(loadedThis, collapse='\n'), "'"))
	}
	else
		jaspResults[["loadedThis"]] <- createJaspHtml(text="No filename to load from was given")
}


