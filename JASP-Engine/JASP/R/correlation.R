
Correlation <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	if (is.null(dataset))
	{
		if (perform == "run") {
			dataset <- read.dataset.to.end()
		} else {
			dataset <- read.dataset.header()
		}
	}

	results <- list()
	
	correlations <- list()
	
	correlations[["title"]] <- "Correlation"
	correlations[["cases"]] <- options$variables
	
	fields <- list()
	rows <- list()
	v.c <- length(options$variables)
	
	if (v.c > 0) {
	
		if (perform == "run") {
		
			reduced.dataset <- subset(dataset, select=unlist(options$variables))
			cor.table <- cor(reduced.dataset)
			
		} else {
		
			cor.table <- matrix(data=".", nrow=v.c, ncol=v.c)
		}
	
		for (i in 1:v.c)
		{
			variable.name <- options$variables[[i]]
			fields[[i]] <- list(id=variable.name, type="number", format="dp:3")
						
			cells <- ifelse(c(rep(TRUE, v.c - i + 1), rep(FALSE, i - 1)), TRUE, FALSE)
			row   <- as.list(cor.table[i,])
			#row[ ! cells] <- ""
			row[ is.na(row) ] <- "NaN"

			
			names(row) <- options$variables
			rows[[i]] <- row
		}
		
	}
		
	schema <- list(fields=fields)
	
	correlations[["schema"]] <- schema
	correlations[["data"]] <- rows
	
	results[["correlations"]] <- correlations

	results
}

