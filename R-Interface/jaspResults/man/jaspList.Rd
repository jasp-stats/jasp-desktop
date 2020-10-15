\name{jaspList}
\Rdversion{1.1}
\docType{class}
\alias{jaspList}

\title{Class "jaspList"}
\description{
  This class represents a list of strings, ints, doubles or booleans/logicals. It is not used directly but exists mainly as a part of \link{jaspTable}. 
  It can be indexed through a fieldname and/or a numerical index.
}
\section{Methods}{
	\describe{
		\item{\code{add(element)}}{Adds 'element' to the end of the indexable list. }
		\item{\code{[[fieldName]]}:}{Retrieves whatever is stored under 'fieldName' in the mapping. }
		\item{\code{[[fieldName]] <- element}:}{Stores 'element' under 'fieldName' in the mapping. }

		\item{\code{[[numericalIndex]]}:}{Retrieves whatever is stored under 'numericalIndex' in the contiguous list. }
		\item{\code{[[numericalIndex]] <- element}:}{Stores 'element' in the contiguous list under index 'numericalIndex'.}
		
	}
}

\section{Extends}{
Class \code{"\link{jaspObject}"}
}

\examples{
# Adding a title
table$colNames[[0]] <- "niceName"
table$colTitles[["niceName"]] <- "Gorgeous Title"
}


\keyword{classes}
