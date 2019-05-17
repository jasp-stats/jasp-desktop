testFundc <- function(jaspResults, dataset, options)
{
	tafel 						<- createJaspTable(title="Ik ben een test");
    jaspResults[['testTafel']]  <- tafel
    
	tafel$addColumnInfo(name="kol0", title="kolom 0!")
	tafel$addColumnInfo(name="kol1", title="kolom 1!")

	#tafel$addColumnInfo(name="kol2")
	tafel$addRows(list(kol0=1, 2, 3))
	tafel$addRows(list(kol0=1, 2, 3))
	#tafel$addRows(list(j=1, 2, 3))
	#tafel$addRows(list(1, 2, 3))
}
