#Some functions that act as a bridge between R and JASP. If JASP isn't running then all columnNames are expected to not be encoded

encodeColumnName <- function(columnNames)
{
  if(is.null(columnNames) | !exists('.encodeColumnName'))
    return(columnNames);

  encoded <- c()
  for(columnName in columnNames)
    if(is.character(columnName))  encoded[length(encoded)+1] <- .encodeColumnName(columnName)
    else                          encoded[length(encoded)+1] <- columnName

  return(encoded)
}

decodeColumnName <- function(columnName)
{
  if(is.null(columnNames) | !exists('.decodeColumnName'))
    return(columnNames);

  decoded <- c()
  for(columnName in columnNames)
    if(is.character(columnName))  decoded[length(decoded)+1] <- .decodeColumnName(columnName)
    else                          decoded[length(decoded)+1] <- columnName

  return(decoded)
}

encodeAllColummnNames <- function(texts)
{
  if(is.null(texts) | !exists('.encodeAllColummnNames'))
    return(texts);

  encoded <- c()
  for(text in texts)
    if(is.character(text))  encoded[length(encoded)+1] <- .encodeAllColummnNames(text)
    else                    encoded[length(encoded)+1] <- text

  return(encoded)
}

decodeAllColummnNames <- function(texts)
{
  if(is.null(texts) | !exists('.decodeAllColummnNames'))
    return(texts);

  decoded <- c()
  for(text in texts)
  if(is.character(text))  decoded[length(decoded)+1] <- .decodeAllColummnNames(text)
  else                    decoded[length(decoded)+1] <- text

  return(decoded)
}
