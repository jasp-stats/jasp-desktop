# Frames with encoded colnames: the R Commander (data/filteredData), filters and
# computed columns all receive data.frames whose colnames are encoded JaspColumn
# tokens. Bare references (data$x, `x`, data$x.nominal) are rewritten by
# ColumnEncoder::encodeRScript before the code is evaluated, but string literals are
# deliberately left alone so that strings keep meaning data. The [[ method below
# resolves such references at lookup time: a string that names a column is encoded
# on the fly, so data[["x"]] and data[["x.nominal"]] work; an already-encoded name
# passes through unchanged; and anything else is left for R to handle, so missing
# columns simply give NULL as they always do.

.encodedFrame <- function(dataFrame)
{
	class(dataFrame) <- c("jaspEncodedFrame", class(dataFrame))
	dataFrame
}

`[[.jaspEncodedFrame` <- function(x, i, ..., exact = TRUE)
{
	if(is.character(i) && length(i) == 1L && !is.na(i))
	{
		encoded <- tryCatch(.encodeColNamesStrict(i), error = function(e) NULL)

		if(!is.null(encoded))
			i <- encoded
		#else i is either already an encoded colname (passed through unchanged) or not
		#a column at all: NextMethod then does exactly what R would have done anyway.
	}

	NextMethod("[[", x, i, ..., exact = exact)
}
