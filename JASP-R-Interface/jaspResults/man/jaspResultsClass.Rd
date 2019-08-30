\name{jaspResultsClass}
\Rdversion{1.1}
\docType{class}
\alias{jaspResultsClass}
\alias{jaspResults}

\title{Class "jaspResultsClass"}
\description{
  This class is the base of the \code{jaspResults} object that is used to store all results from an analysis and which, in JASP, sends data to the resultsview.
}
\section{Creation}{
  \describe{
		\item{\code{initJaspResults()}:}{When you are running your code seperately from JASP you can call initJaspResults() to have 'jaspResults' be created. 
		If you are making an analysis in JASP you do not need to do anything as jaspResults will be a given to your analysis.}
	}
}
\section{Methods}{
  \describe{

		\item{\code{length()}:}{Returns how many objects are stored in this container. (inherited from jaspContainer)}
		\item{\code{[[}:}{Retrieve an object from this container as specified under the fieldname. (inherited from jaspContainer)}
		\item{\code{[[<-}:}{Insert an object into this container under a field-name, if this object is a jaspObject and without a title it will get the field-name as title. (inherited from jaspContainer)}

    \item{\code{setOptions}:}{Uses the passed list as current options. This is usually passed by JASP directly but could be useful to set when developing your analysis, aka in jaspTools. }
    \item{\code{changeOptions}:}{Changes the currently set options and removes all objects that depend on the changed options. Mostly useful for unit tests because this we we can simulate re-running the analysis. Should not be used in an analysis!}

		\item{\code{getKeepList}:}{Internal function, generates a list of all files that should be kept, aka plot-images, statefile and save location of the results themselves.}
		\item{\code{getPlotObjectsForState}:}{Internal function.}

		\item{\code{setErrorMessage}:}{ Sets an errormessage on the results, should not be called from an analysis. You can set errors on plots and tables directly.}


		\item{\code{complete}:}{Calls \code{send} and changes status to complete and stores itself next to the state. This should not be called by a user, JASP will take care of it,}
		\item{\code{send}:}{Send results to JASP but obviously only when running the analysis *in* JASP.}
	 }
}

\section{Fields}{
  \describe{
    \item{\code{relativePathKeep}:}{Location of the temporary files, only for internal use in JASP.}
  }
}
\section{Extends}{
Class \code{"\link{jaspContainer}"}
}

\examples{
#creating a table in the results:
jaspResults[["stats"]] <- createJaspTable("Descriptives")

# Checking if the dependencies removed the object and creating it again if necessary:
if(is.null(jaspResults[["stats"]])) 
	jaspResults[["stats"]] <- makeANiceDescriptivesTableFunction()
}

\keyword{classes}
