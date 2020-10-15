\name{jaspContainer}
\Rdversion{1.1}
\docType{class}
\alias{jaspContainer}
\alias{createJaspContainer}

\title{Class "jaspContainer"}
\description{
  This class represents an ordered collection of jaspObjects, it allows access to these objects through [[]] indexing.
}
\section{Creation}{
  \describe{
		\item{\code{createJaspContainer(title="", dependencies=NULL)}:}{Creates a jaspContainer with the set title. If the title is "" then, when it is placed in a container(/results), it will use it's fieldname.
		It will only show up in your results if it or one of it's parents is jaspResults.}
	}
}
\section{Methods}{
  \describe{

		\item{\code{length()}:}{Returns how many objects are stored in this container.}
		\item{\code{[[}:}{Retrieve an object from this container as specified under the fieldname.}
		\item{\code{[[<-}:}{Insert an object into this container under a fieldname, if this object is a jaspObject and without a title it will get the fieldname as title.}
	 }
}	

\section{Extends}{
Class \code{"\link{jaspObject}"}
}

\examples{
#creating a container for plots and addin some plots:
cont <- jaspContainer("plots")
cont[["plotA"]] <- createJaspPlot(plot=SomeFunc())
cont[["plotB"]] <- createJaspPlot(plot=SomeOtherFunc())

cont[["plotA"]]$print() # prints the plot object that was stored earlier under fieldname 'plotA'

}


\keyword{classes}
