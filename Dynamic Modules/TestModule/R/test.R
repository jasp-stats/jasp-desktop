testFunc <- function(jaspResults, dataset, options)
{
	tafel 						<- createJaspTable(title="Ik ben een test");
    jaspResults[['testTafel']]  <- tafel
    
	tafel$addColumnInfo(name="kol0", title="kolom 0!")
	tafel$addColumnInfo(name="kol1", title="kolom 1!")

	#tafel$addColumnInfo(name="kol2")
	tafel$addRows(list(kol0=1, kol1=2, 3), rowNames="a")
	tafel$addRows(as.data.frame(list(kol0=1, 2, 3)), rowNames="b")
	tafel$addRows(as.data.frame(list('a', 'b','c')), rowNames="b")
	tafel$addRows(as.data.frame(list('a', 'b','c')), rowNames="c")
	#tafel$addRows(list(j=1, 2, 3))
	#tafel$addRows(list(1, 2, 3))

	tafel$addFootnote(message="msg 1", symbol="", colNames="kol0", rowNames="b")
	tafel$addFootnote(message="msg 2", symbol="", colNames="kol0")
	tafel$addFootnote(message="msg 3", symbol="")

	tafel$addFootnote(message="msg 4", colNames="kol1", rowNames="a")
	tafel$addFootnote(message="msg 5 moet voor 4", colNames="kol0", rowNames="a")
	tafel$addFootnote(message="msg 6 moet na 1", colNames="kol1", rowNames="b")

	filtered <- createJaspTable(title="Received Data")
	filtered$addColumnInfo(name="rowName", 	title="Row")
	filtered$addColumnInfo(name="data", 	title=options$filteredData$colName)

	filtered[['data']]    <- options$filteredData[[1]]$values
	filtered[['rowName']] <- options$filteredData[[1]]$rowIndices

	jaspResults[['filteredDataEntryExample']] <- filtered
}
