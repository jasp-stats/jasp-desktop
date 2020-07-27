testPlotStatus <- function(jaspResults, dataset, options)
{
	plot                  <- createJaspPlot(title="hahahah", dependencies=c('checkbox_change'))
	jaspResults[['plot']] <- plot

	print('Sleep a couple of seconds')
	Sys.sleep(3)

	print('Status to running and sleep some more')
	plot$status <- "running"
	Sys.sleep(3)

	print('Status to waiting and sleep some more')
	plot$status <- "waiting"
	Sys.sleep(3)

	print('Status back to running and sleep some more')
	plot$status <- "running"
	Sys.sleep(3)


	print('Status to complete and sleep some more')
	plot$status <- "complete"
	Sys.sleep(3)

	print("Done!") 
}