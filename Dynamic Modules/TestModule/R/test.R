testFunc <- function(jaspResults, dataset, options)
{
  containerA <- createJaspContainer(title="A", position=1)
  containerB <- createJaspContainer(title="B", position=1)
  containerC <- createJaspContainer(title="C", position=2)

  containerA[["B"]] <- containerB
  containerA[["C"]] <- containerC

  jaspResults[["A"]] <- containerA

	tafel 						<- createJaspTable(title="Ik ben een test", position=5);

  containerA[["Tssss"]] <- tafel

  tafel2 <- createJaspTable(title="Ik ben nog een test", position=-5);
  containerC[["T"]] <- tafel2

  tafel2$setExpectedSize(3, 3)
  tafel2$setError("AAAAAH!")
    
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
}
