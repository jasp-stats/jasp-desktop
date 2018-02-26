\name{jaspHtml}
\Rdversion{1.1}
\docType{class}
\alias{jaspHtml}

\title{Class "jaspHtml"}
\description{
  This class represents a single html-element in the results. It is useful for specifying headers/loose titles/paragraphs or whatever.
}
\section{Creation}{
  \describe{
		\item{\code{createJaspHtml(text="", elementType="p")}:}{
			Creates a jaspHtml, by default it is an empty paragraph but a useful different type is "h1" to "h5" for progressively smaller headers.
		}
	}
}	
\section{Properties}{
	\describe{
		\item{\code{text}:}{The text contained in the html element. }
		\item{\code{elementType}:}{The type of html element. }
		
	}
}

\section{Extends}{
Class \code{"\link{jaspObject}"}
}

\examples{
# Adding a title
jaspResults[["someTitle"]] <- createJaspHtml("Announcement", "h2")

# Adding a text
jaspResults[["someText"]] <- createJaspHtml("These results are very <i>verbose</i> because of the inclusion of html elements!", "p")
}


\keyword{classes}
