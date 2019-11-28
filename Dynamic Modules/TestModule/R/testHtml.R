testHtmlFunc <- function(jaspResults, dataset, options)
{
	elementType <- 'p'

	if(options$checkbox_error) 
		elementType <- 'errorMsg'

	jaspResults[["errorMsg"]] <- createJaspHtml(text='<p>hallo wereld!</p>', elementType=elementType)

	jaspResults[['boodschap']] <- createJaspHtml(text=paste0('state contains: ', jaspResults[['state']]$object))
}