\name{jaspTable}
\Rdversion{1.1}
\docType{class}
\alias{jaspTable}
\alias{createJaspTable}

\title{Class "jaspTable"}
\description{
  This class represents a table in the results.
}
\section{Creation}{
  \describe{
    \item{\code{createJaspTable(title="", data=NULL, colNames=NULL, colTitles=NULL, overtitles=NULL, colFormats=NULL, rowNames=NULL, rowTitles=NULL, dependencies=NULL)}:}{
			Creates a jaspTable with the set values. If the title is "" then, when it is placed in a container(/results), it will use it's fieldname.
			It will only show up in your results if it or one of it's parents is jaspResults.
			Any argument passed in data will be processed as if 'setData' was used, the rest will be passed through to the corresponding 'set'-functions (aka setColNames for colNames etc). }
	}
}
\section{Methods}{
  \describe{
      \item{\code{addColumnInfo(name=NULL, title=NULL, overtitle=NULL, type=NULL, format=NULL, combine=NULL)}:}{Adds a column to the table with the specified properties, this is not strictly necessary but it gives you control on the order and look of your table in an easy way.
		  All arguments are optional but it makes sense to give your columns a name and a title at the very least. It is easier to use this method then directly accessing colNames and the others. }

		\item{\code{setData(data)}:}{Set the data of the table, this methods accepts lists, dataframes, matrices and vectors. If any column- or rownames are specified they are set on the object, but only if they aren't set yet. Any one-dimensional data given will be assumed to be a row.}

    \item{\code{addRows(rows, rowNames=null}:}{Add one or more rows to the table, where 'rows' is a list, dataframe, matrix or vector.
      Before the data is added all existing columns will be made the same length by appending null-values.
      If the new data contains more columns than currently present empty columns will be added.
      Columnnames will be extracted and used to place the data in the correct column, they can be specified through the elementnames of a list, names of a data.frame and colnames of a matrix.
      To also set the rownames you can fill pass a characterVector with the desired names in the second argument.}
    \item{\code{addColumns(cols)}:}{Add one or more columns to the table, where 'cols' is a list, dataframe, matrix or vector.
			A list is interpreted as being a single column where each elementname is a rowname. A dataframe already consists of columns so these are simply used as expected, the same goes for a matrix. A vector is of course also a column and the rownames are used.
			To specifically set a vector or list with a desired name you can directly set it through: [['columnName']]<-
		}
		\item{\code{[['columnName']]<-'column'}:}{Add or set a column in the table, where 'column' is a list or vector.
			Each elementname in 'column' is a rowname that is set in the table if nothing was set yet.
			If a column with 'columnName' exists it is overridden with 'column' else it is added to the end of the columnList.
		}
		
		\item{\code{addFootnote(message="", symbol=NULL, col_names=NULL, row_names=NULL)}:}{Add a footnote to your table with 'message'. If 'symbol' is specified it will be used. 
		If col_names is specified then the footnote will point to that/those column(s). If row_names is specified the same for rows. If both col_names and row_names are specified the footnote will point to specific cells.}

    \item{\code{setColName(columnIndex, newName)}:}{Set new name on index.}
    \item{\code{setColType(columnName, newType)}:}{Set new type for columnName.}
    \item{\code{setColTitle(columnName, newTitle)}:}{Set new title for columnName.}
    \item{\code{setColOvertitle(columnName, newOvertitle)}:}{Set new overtitle for columnName.}
    \item{\code{setColFormat(columnName, newFormat)}:}{Set new format for columnName.}
    \item{\code{setColCombine(columnName, newCombine)}:}{Set new combine for columnName.}
    \item{\code{setRowName(rowIndex, newName)}:}{Set new name for row index.}
    \item{\code{setRowTitle(rowName, newTitle)}:}{Set new title for rowName.}

    \item{\code{setColName(columnIndex)}:}{Get name on index.}
    \item{\code{setColType(columnName)}:}{Get type for columnName.}
    \item{\code{setColTitle(columnName)}:}{Get title for columnName.}
    \item{\code{setColOvertitle(columnName)}:}{Get overtitle for columnName.}
    \item{\code{setColFormat(columnName)}:}{Get format for columnName.}
    \item{\code{setColCombine(columnName)}:}{Get combine for columnName.}
    \item{\code{setRowName(rowIndex)}:}{Get name for row index.}
    \item{\code{setRowTitle(rowName)}:}{Get title for rowName.}

    \item{\code{setExpectedSize}:}{Set the expected size of this table to the specified columnCount and rowCount. It will make your table show up, filled with dots, at this size and as you add data the dots will be replaced with it.}
    \item{\code{setExpectedColumns}:}{Set the expected size of this table to the specified columnCount. It will make your table show up, filled with dots, at this size and as you add data the dots will be replaced with it.}
    \item{\code{setExpectedRows}:}{Set the expected size of this table to the specified rowCount. It will make your table show up, filled with dots, at this size and as you add data the dots will be replaced with it.}
	 }
}	
\section{Properties}{
	\describe{
		\item{\code{transpose}:}{The table will be diplayed transposed in the results. }
		\item{\code{transposeWithOvertitle}:}{If transpose is TRUE the table will use the first column as an overtitle. }
		\item{\code{showSpecifiedColumnsOnly}:}{If set to true will make only the specified columns (through addColumnInfo etc) show in the results. }

    \item{\code{status}:}{The status of the table, by default and usually "running", will be set to "complete" automatically when the analysis ends. But if you know you are done already with it you can set it manually as well. }
	}
}

\section{Extends}{
Class \code{"\link{jaspObject}"}
}

\examples{
# How setting up a table and modifying the data works, some examples: 
t <- createJaspTable()
t$addColumnInfo(name="str", title="Strings")
t$addColumnInfo(name="num", title="Numbers")

kolA <- c(1,2,3,4)
kolB <- c("a", 'b', 'c', 'd')
df <- data.frame(kolA, kolB)
names(df) <- c("num", "str")

# df:
#  num str
#   1   a
#   2   b
#   3   c
#   4   d


t$setData(df)

# t:
# colnames:	str		num
# colTitles:	Strings		Numbers
#		"a"		1
#		"b"		2
#		"c"		3
#		"d"		4

t$addColumns(c(5,4,3,2,1))

# t:
# colnames:	str		num		col_2
# colTitles:	Strings		Numbers	
#		"a"		1		5
#		"b"		2		4
#		"c"		3		3
#		"d"		4		2
#						1

t$addRows(list(num="hallo", str=2, "iets"))

# t:
# colnames:	str		num		col_2
# colTitles:	Strings		Numbers	
#		"a"		1		5
#		"b"		2		4
#		"c"		3		3
#		"d"		4		2
#		NULL		NULL		1
#		2		"hallo"		"iets"

t[["col_2"]] <- c(1,2,3)

# t:
# colnames:	str		num		col_2
# colTitles:	Strings		Numbers	
#		"a"		1		1
#		"b"		2		2
#		"c"		3		3
#		"d"		4		
#		NULL		NULL	
#		2		"hallo"	

t$colNames[[3]] <- "thirdCol"
t$addRows(list(thirdCol=99, num="world", str=""))

# t:
# colnames:	str		num		thirdCol
# colTitles:	Strings		Numbers	
#		"a"		1		1
#		"b"		2		2
#		"c"		3		3
#		"d"		4		NULL
#		NULL		NULL		NULL
#		2		"hallo"		NULL
#		""		"world"		99

}


\keyword{classes}
