\name{jaspState}
\Rdversion{1.1}
\docType{class}
\alias{jaspState}
\alias{createJaspState}

\title{Class "jaspState"}
\description{
  This class represents a single R Object to be kept for the next reviosn of your analysis. It is a replacement for the state. You can store objects in it and specify dependencies, if you dont it will always be thrown away so 
}
\section{Creation}{
  \describe{
		\item{\code{createJaspState(object=NULL, title="", dependencies=NULL)}:}{
			Creates a jaspState, containing nothing by default and being without a title. 
		}
	}
}	
\section{Properties}{
	\describe{
		\item{\code{object}:}{The object to be stored in or retrieved from state. Be sure to not access the stored object very often, because it will be unserialized every single time it is retrieved. }	
	}
}

\section{Extends}{
Class \code{"\link{jaspObject}"}
}

\examples{
# Storing something in the state.
jaspResults[["someTitle"]] <- createJaspState(list(data=c(1,2,3), "A list", c("optionA", "optionB")) #the list is serialized and stored in jaspResults
					      
previouslyStoredTitle <- jaspResults[["someTitle"]]$object #the stored list is unserialized again!					      
}


\keyword{classes}
