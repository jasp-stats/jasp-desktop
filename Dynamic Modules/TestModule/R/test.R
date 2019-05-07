testFunc <- function(jaspResults, dataset, options)
{
    jaspResults[['testTafel']] <- createJaspTable(title="Ik ben geen test tafel! ;-P")
    jaspResults[['testTafel']]$addRows(list(rijA=c(1,2,3,4), rijB=c(Inf, -Inf, NA, NaN)))
    jaspResults[['testTafel']]$addRows(list(NULL,2))
  #  jaspResults[['testTafel']]$addRows(c(1))
    jaspResults[['testTafel']]$addRows(list(1,2,NULL, 6)) 

    titel0 <- '0?'
    if(is.null(jaspResults[["check0"]]))
    {
      titel0 <- 'check0 bestaat niet! Dus die maken we nu'

      jaspResults[["check0"]] <- createJaspTable(title="Check 0 tafel!", dependencies=c('checkbox_0'))
      jaspResults[["check0"]]$setExpectedSize(5, 5)
    }
    else
      titel0 <- 'check0 bestaat!'

    jaspResults[["check0"]]$title <- titel0

    jaspResults[["check0"]]$title <- paste0(jaspResults[["check0"]]$title)

    titel1 <- '1?'
    if(is.null(jaspResults[["check1"]]))
    {
      titel1 <- 'check1 bestaat niet! Dus die maken we nu'

      jaspResults[["check1"]] <- createJaspTable(title='Check 1 tafel!', dependencies=c('checkbox_1'))
      jaspResults[["check1"]]$setExpectedSize(5, 5)
    }
    else
      titel1 <- 'check1 bestaat!'

    jaspResults[["check1"]]$title <- titel1

    jaspResults[["klikMe"]] <- createJaspHtml('<a href="http://jasp-stats.org">jasp!</a>')
}