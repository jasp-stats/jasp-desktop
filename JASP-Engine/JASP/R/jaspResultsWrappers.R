###########################
## JASP Results Wrappers ##
###########################
# should define the same functions as those in zzzWrappers.R in jaspResults package.
# with the difference that here they should directly point to jaspResultsModule

# if title is left unset (aka "") then, when added to a container/results, it will take the fieldname as title.
# width and height should be set to some global default setting and only in exceptional cases manually. Maybe we could take it from JASPplot?
# aspectRatio of > 0 sets height to width * aspectRatio.
createJaspPlot <- function(plot=NULL, title="", width=320, height=320, aspectRatio=0, error=NULL, errorMessage="", dependencies=NULL, position=NULL)
{
  jaspPlotObj  <- jaspResultsModule$create_cpp_jaspPlot(title) # If we use R's constructor it will garbage collect our objects prematurely.. #new(jaspResultsModule$jaspPlot, title)

  if(aspectRatio > 0 && !is.null(width) && width != 0)  height = aspectRatio * width
  else if(aspectRatio > 0)                              width = height / aspectRatio;

  jaspPlotObj$width  <- width
  jaspPlotObj$height <- height
  jaspPlotObj$aspectRatio <- aspectRatio

  if(!is.null(error) || errorMessage != "")
  {
    if(is.null(error))  jaspPlotObj$error <- "errorMsgSet"
    else                jaspPlotObj$error <- error
    jaspPlotObj$errorMessage  <- errorMessage
  }

  jaspPlotObj$plotObject <- plot

  if(!is.null(dependencies))
    jaspPlotObj$dependOnOptions(dependencies)

  if(is.numeric(position))
    jaspPlotObj$position = position

  return(jaspPlotObj)
}

createJaspContainer <- function(title="", dependencies=NULL, position=NULL)
{
  container <- jaspResultsModule$create_cpp_jaspContainer(title) # If we use R's constructor it will garbage collect our objects prematurely.. #new(jaspResultsModule$jaspContainer, title))

  if(!is.null(dependencies))
    container$dependOnOptions(dependencies)

  if(is.numeric(position))
    container$position = position

  return(container)
}

createJaspTable <- function(title="", data=NULL, colNames=NULL, colTitles=NULL, overtitles=NULL, colFormats=NULL, rowNames=NULL, rowTitles=NULL, dependencies=NULL, position=NULL)
{
  jaspObj <- jaspResultsModule$create_cpp_jaspTable(title) # If we use R's constructor it will garbage collect our objects prematurely.. #new(jaspResultsModule$jaspTable, title)

  if(!is.null(data))
    jaspObj$setData(data)

  if(!is.null(colNames))
    jaspObj$setColNames(colNames)

  if(!is.null(colTitles))
    jaspObj$setColTitles(colTitles)

  if(!is.null(overtitles))
    jaspObj$setColOvertitles(overtitles)

  if(!is.null(colFormats))
    jaspObj$setColFormats(colFormats)

  if(!is.null(rowNames))
    jaspObj$setRowNames(rowNames)

  if(!is.null(rowTitles))
    jaspObj$setRowTitles(rowTitles)

  if(!is.null(dependencies))
    jaspObj$dependOnOptions(dependencies)

  if(is.numeric(position))
    jaspObj$position = position

  return(jaspObj)
}

createJaspHtml <- function(text="", elementType="p", class="", dependencies=NULL, title="hide me", position=NULL) # if you change "hide me" here then also change it in zzzWrappers.R and in HtmlNode.js or come up with a way to define it in such a way to make it show EVERYWHERE...
{
  htmlObj             <- jaspResultsModule$create_cpp_jaspHtml(text) # If we use R's constructor it will garbage collect our objects prematurely.. #
  htmlObj$elementType <- elementType
  htmlObj$class       <- class
  htmlObj$title       <- title

  if(!is.null(dependencies))
    htmlObj$dependOnOptions(dependencies)

  if(is.numeric(position))
    htmlObj$position = position

  return(htmlObj)
}

createJaspState <- function(object=NULL, title="", dependencies=NULL, position=NULL)
{
  stateObj <- jaspResultsModule$create_cpp_jaspState(title) # If we use R's constructor it will garbage collect our objects prematurely.. #

  stateObj$object <- object

  if(!is.null(dependencies))
      stateObj$dependOnOptions(dependencies)

  if(is.numeric(position))
    stateObj$position = position

  return(stateObj)
}
