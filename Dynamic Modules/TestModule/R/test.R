testFunc <- function(jaspResults, dataset, options)
{
    jaspResults[['testTafel']] <- createJaspTable(title="Ik ben een test tafel!")
    jaspResults[['testTafel']]$addRows(list(rijA=c(1,2,3,4), rijB=c(Inf, -Inf, NA, NaN)))
    jaspResults[['testTafel']]$addRows(list(NULL,2))
  #  jaspResults[['testTafel']]$addRows(c(1))
    jaspResults[['testTafel']]$addRows(list(1,2,NULL, 6)) 
}
