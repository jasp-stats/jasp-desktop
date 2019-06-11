\name{jaspPlot}
\Rdversion{1.1}
\docType{class}
\alias{jaspPlot}
\alias{createJaspPlot}

\title{Class "jaspPlot"}
\description{
  This class represents a plot in the results. 
}
\section{Creation}{
  \describe{
		\item{\code{createJaspPlot(plot=NULL, title="", width=320, height=320, aspectRatio=0, error=NULL, errorMessage="", dependencies=NULL)}:}{
			Creates a jaspPlot based primarily on whatever is passed in 'plot'. 'plot' can be an R-function to render a graph or preferably a ggplot2/jaspGraphs plot-object. It can be NULL but this is mostly useful to show errors. 
			If the title is "" then, when it is placed in a container(/results), it will use it's fieldname.
			It will only show up in your results if it or one of it's parents is jaspResults.
		}
	}
}
\section{Methods}{
  \describe{
		\item{\code{addFootnote(message="", symbol=NULL)}:}{Add a footnote to your plot with 'message'. If 'symbol' is specified it will be used.}
		
		\item{\code{setPlotObjSerialized}:}{Stores the plot-object as a serialized R-object. It is for internal use only. }
	 }
}	
\section{Properties}{
	\describe{
		\item{\code{aspectRatio}:}{Stores the aspect ratio used to make the plot, will not redraw the plot on change. }
		\item{\code{width}:}{Stores the width used to make the plot, will not redraw the plot on change. }
		\item{\code{height}:}{Stores the height used to make the plot, will not redraw the plot on change. }

	    \item{\code{status}:}{Stores the status of the plot, default is complete, set to 'running' if it takes a long time to calculate it. }

		\item{\code{filePathPng}:}{The location of the generated png (eps?) file that should be remembered in tempfiles. For internal use mostly. }
	
	}
}

\section{Extends}{
Class \code{"\link{jaspObject}"}
}

\examples{
# a generic plot example that depends on two options:
p <- createJaspPlot(plot=somePlottingFunc(), dependencies=c("plotSomething", "plotColours"))
}


\keyword{classes}
