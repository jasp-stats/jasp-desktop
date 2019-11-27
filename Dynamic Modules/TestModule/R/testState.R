testStateFunc <- function(jaspResults, dataset, options)
{

	if(is.null(jaspResults[['state']]))
  		jaspResults[['state']] <- createJaspState(options$saveMe, dependencies=c("checkbox_0", "saveMe"))

	jaspResults[['boodschap']] <- createJaspHtml(text=paste0('state contains: ', jaspResults[['state']]$object))
}


