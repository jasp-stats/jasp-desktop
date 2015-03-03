$.widget("jasp.table", {

	options: {
		title: "",
		subtitle: null,
		variables: [ ],
		data: [ ],
		casesAcrossColumns : false,
		formats : null,
		status : "waiting",
		footnotes : [ ],
		citation : null
	},
	_create: function () {
		this.element.addClass("jasp-table")
		this.refresh()
	},
	_setOptions: function (options) {
		this._super(options)
		
		this.refresh()
	},
	_fsd : function(value) { // first significant digit position
	
		if (value > 0)
			return Math.floor(Math.log(+value) / Math.log(10)) + 1
		else if (value < 0)
			return Math.floor(Math.log(-value) / Math.log(10)) + 1
		else
			return 1
	
	},
	_fsdoe : function(value) { // first significant digit position in the exponent in scientific notation
	
		if (value == 0)
			return 1

		var exponent = Math.floor(Math.log(Math.abs(value)) / Math.log(10))

		return this._fsd(exponent)
	
	},
	_symbol : function(index) {
	
		// no c in webkit?!

		return ("\u1D43\u1D47" /* \u1D9C */ + "\u1D48\u1D49\u1DA0\u1D4D\u02B0\u2071\u02B2\u1D4F\u02E1\u1D50\u207F\u1D52\u1D56\u02B3\u02E2\u1D57\u1D58\u1D5B\u02B7\u02E3\u02B8\u1DBB").charAt(index)
	
	},
	_swapRowsAndColumns : function(columnHeaders, columns) {
	
		var newRowCount = columns.length - 1
		var newColumnCount = columns[0].length + 1

		var newColumnHeaders = columns[0]
		var newColumns = Array(newColumnCount)

		var cornerCell = columnHeaders.shift()
		newColumnHeaders.unshift(cornerCell)
		newColumns[0] = columnHeaders
		
		for (var colNo = 1; colNo < newColumnCount; colNo++) {
		
			newColumns[colNo] = Array(newRowCount)
		
			for (var rowNo = 0; rowNo < newRowCount; rowNo++) {
			
				newColumns[colNo][rowNo] = columns[rowNo+1][colNo-1]
			}
		
		}
		
		return { columnHeaders : newColumnHeaders, columns : newColumns, rowCount : newRowCount, columnCount : newColumnCount }
	
	},
	_formatColumn : function(column, type, format, alignNumbers, combine) {

		var columnCells = Array(column.length)

		if (type == "string" || typeof format == "undefined") {
		
			for (var rowNo = 0; rowNo < column.length; rowNo++) {

				var clazz = (type == "string") ? "text" : "number"
				
				var cell = column[rowNo]
				var content = cell.content
				var formatted
				var combined = false
				
				if (typeof content == "undefined") {
				
					formatted = { content : "." }
				}
				else if (combine && rowNo > 0 && column[rowNo-1].content == content) {
				
					formatted = { content : "&nbsp;", class : clazz }
					combined = true
				}
				else {
				
					if (typeof content === "string")
						content = content.replace(/\u273B/g, "<small>\u273B</small>")
					
					formatted = { content : content, "class" : clazz }
				}
				
				if (combined == false && cell.isStartOfGroup)
					formatted.isStartOfGroup = true
					
				if (cell.isEndOfGroup)
					formatted.isEndOfGroup = true
					
				if (typeof cell.footnotes != "undefined")
					formatted.footnotes = this._getFootnotes(cell.footnotes)
					

					
				columnCells[rowNo] = formatted
			}
			
			return columnCells
		}

		var formats = format.split(";");
		var p = NaN
		var dp = NaN
		var sf = NaN
		var pc = false
		var approx = false

		for (var i = 0; i < formats.length; i++) {
		
			var f = formats[i]
			
			if (f.indexOf("p:") != -1)
				p = f.substring(2)

			if (f.indexOf("dp:") != -1)
				dp = f.substring(3)
			
			if (f.indexOf("sf:") != -1)
				sf = f.substring(3)
				
			if (f.indexOf("pc") != -1)
				pc = true
				
			if (f.indexOf("~") != -1)
				approx = true;
		}
		
		if (isFinite(sf)) {

			var upperLimit = 1e6
			var minLSD = Infinity	// right most position of the least significant digit
			var maxFSDOE = -Infinity  // left most position of the least significant digit of the exponent in scientific notation
			
			for (var rowNo = 0; rowNo < column.length; rowNo++) {

				var cell = column[rowNo]
				var content = cell.content
				
				if (isNaN(parseFloat(content)))  // isn't a number
					continue

				var fsd   = this._fsd(content)  // position of first significant digit
				var lsd   = fsd - sf
				
				if (Math.abs(content) >= upperLimit || Math.abs(content) <= Math.pow(10, -dp)) {
				
					var fsdoe = this._fsdoe(content)
					if (fsdoe > maxFSDOE)
						maxFSDOE = fsdoe
				}
				else if (lsd < minLSD) {
				
					minLSD = lsd
				}
			}
			
			if (minLSD < -dp)
				minLSD = -dp
			if (minLSD > 0)
				minLSD = 0
			else if (minLSD < -20)
				minLSD = -20
		
			for (var rowNo = 0; rowNo < column.length; rowNo++) {

				var cell = column[rowNo]
				var content = cell.content
				var formatted
				
				if (typeof content == "undefined") {
				
					formatted = { content : "." }
				}
				else if (typeof content === "") {
				
					formatted = { content : "&nbsp;", "class" : "number" }
				}
				else if (combine && rowNo > 0 && column[rowNo-1].content == content) {
				
					formatted = { content : "&nbsp;", "class" : "number" }
				}
				else if (isNaN(parseFloat(content))) {  // isn't a number
					
					formatted = { content : content, "class" : "number" }

				}
				else if (content < p) {
					
					formatted = { content : "<&nbsp" + p, "class" : "p-value" }

				}
				else if (content == 0) {

					var zero = 0

					if (isFinite(dp))
						formatted = { content : zero.toFixed(dp), "class" : "number" }
					else
						formatted = { content : zero.toPrecision(sf), "class" : "number" }
					
				}
				else if (Math.abs(content) >= upperLimit || Math.abs(content) <= Math.pow(10, -dp)) {
				
					var exponentiated = content.toExponential(sf-1).replace(/-/g, "&minus;")
					var paddingNeeded = Math.max(maxFSDOE - this._fsdoe(content), 0)
					
					var split = exponentiated.split("e")
					var mantissa = split[0]
					var exponent = split[1]
					var exponentSign = exponent.substr(0, 1)
					var exponentNum  = exponent.substr(1)

					var padding
					
					if (paddingNeeded)
						padding = '<span class="do-not-copy" style="visibility: hidden;">' + Array(paddingNeeded + 1).join("0") + '</span>'
					else
						padding = ''
					
					var reassembled = mantissa + "e&thinsp;" + padding + exponentSign + exponentNum
				
					formatted = { content : reassembled, "class" : "number" }
				}
				else {
					
					if (alignNumbers) {
					
						formatted = { content : content.toFixed(-minLSD).replace(/-/g, "&minus;"), "class" : "number" }
					}
					else {
					
						formatted = { content : content.toPrecision(sf).replace(/-/g, "&minus;"), "class" : "number" }
					}
				}
				
				if (approx)
					formatted.content = "&#x2248;&thinsp;" + formatted.content
				
				if (typeof cell.footnotes != "undefined")
					formatted.footnotes = this._getFootnotes(cell.footnotes)
					
				if (cell.isStartOfGroup)
					formatted["class"] += " new-group-row"
				
				columnCells[rowNo] = formatted
			}
		}
		else if (isFinite(dp)) {
		
			for (var rowNo = 0; rowNo < column.length; rowNo++) {

				var cell = column[rowNo]
				var content = cell.content
				var formatted
				
				if (typeof content == "undefined") {
				
					formatted = { content : "." }
				}
				else if (content === "") {
				
					formatted = { content : "&nbsp;" }
				}
				else if (combine && rowNo > 0 && column[rowNo-1].content == content) {
				
					formatted = { content : "&nbsp;", "class" : "number" }
				}
				else if (isNaN(parseFloat(content))) {  // isn't a number
					
					formatted = { content : content, "class" : "number" }
				}
				else if (content < p) {
					
					formatted = { content : "<&nbsp" + p, "class" : "p-value" }
				}
				else if (pc) {
				
					formatted = { content : "" + (100 * content).toFixed(dp) + "&thinsp;%", "class" : "percentage" }
				}
				else {
				
					formatted = { content : content.toFixed(dp).replace(/-/g, "&minus;"), "class" : "number" }
				}
				
				if (typeof cell.footnotes != "undefined")
					formatted.footnotes = this._getFootnotes(cell.footnotes)
					
				if (cell.isStartOfGroup)
					formatted["class"] += " new-group-row"
				
				columnCells[rowNo] = formatted
			}
		}
		else if (pc) {
		
			for (var rowNo = 0; rowNo < column.length; rowNo++) {

				var cell = column[rowNo]
				var content = cell.content
				var formatted
				
				if (typeof content == "undefined") {
				
					formatted = { content : "." }
				}
				else if (content === "") {

					formatted = { content : "&nbsp;" }				
				}
				else if (isNaN(parseFloat(content))) {  // isn't a number
					
					formatted = { content : content, "class" : "percentage" }
				}
				else {
				
					formatted = { content : "" + (100 * content.toFixed(0)) + "&thinsp;%", "class" : "percentage" }
				}
				
				if (typeof cell.footnotes != "undefined")
					formatted.footnotes = this._getFootnotes(cell.footnotes)
					
				if (cell.isStartOfGroup)
					formatted["class"] += " new-group-row"
				
				columnCells[rowNo] = formatted
			}			
		}
		else {
		
			for (var rowNo = 0; rowNo < column.length; rowNo++) {
			
				var cell = column[rowNo]
				var content = cell.content
				var formatted
				
				if (typeof content == "undefined") {
				
					formatted = { content : "." }
				}
				else if (content === "") {

					formatted = { content : "&nbsp;" }				
				}
				else if (combine && rowNo > 0 && column[rowNo-1].content == content) {
				
					formatted = { content : "&nbsp;" }
				}
				else {
				
					formatted = { content : content }
				}
				
				if (typeof cell.footnotes != "undefined")
					formatted.footnotes = this._getFootnotes(cell.footnotes)
					
				if (cell.isStartOfGroup)
					formatted["class"] += " new-group-row"
					
				columnCells[rowNo] = formatted
			}
		}
		
		return columnCells
	
	},
	_getFootnotes : function(indices) {
	
		var footnotes = Array(indices.length)

		for (var i = 0; i < indices.length; i++) {
		
			var index = indices[i]
			
			if (_.isString(index)) {
			
				footnotes[i] = index
		
			} else if (index < this.options.footnotes.length) {
		
				var footnote = this.options.footnotes[index]
				if (typeof footnote.symbol == "undefined")
					footnotes[i] = this._symbol(index)
				else if (_.isNumber(footnote.symbol))
					footnotes[i] = this._symbol(footnote.symbol)
				else
					footnotes[i] = footnote.symbol
			}
			else {
			
				footnotes[i] = this._symbol(index)			
			}
			
		}
			
		return footnotes
		
	},
	refresh: function () {

		var columnDefs = this.options.schema.fields
		var columnCount = columnDefs.length
		
		var rowData = this.options.data
		var rowCount = rowData ? rowData.length : 0

		var columnHeaders = Array(columnCount)
		var columns = Array(columnCount)
		

		for (var colNo = 0; colNo < columnCount; colNo++) {
		
			// populate column headers
				
			var columnDef = columnDefs[colNo]
			var columnName = columnDef.name

			var title = columnDef.title
			
			if (typeof title == "undefined") 
				title = columnName
				
			if (title == "")
				title = "&nbsp;"
			
			var columnType = columnDef.type
			
			var columnHeader = { content : title, header : true, type : columnType }

			// At the moment this only supports combining two column headers
			// Support for multiple can be added when necessary

			if (columnDef.combineHeaders) {
			
				if (colNo + 1 < columnCount && columnDefs[colNo + 1].combineHeaders && columnDef.title == columnDefs[colNo + 1].title)
					columnHeader.span = 2
				else if (colNo - 1 >= 0 && columnDefs[colNo - 1].combineHeaders && columnDef.title == columnDefs[colNo - 1].title)
					columnHeader.span = 0
			}
			
			if (typeof columnDef[".footnotes"] != "undefined")
				columnHeader.footnotes = this._getFootnotes(columnDef[".footnotes"])

			columnHeaders[colNo] = columnHeader			
			
			// populate cells column-wise
			
			var column = Array(rowCount)
			var isGrouped = false
			
			for (var rowNo = 0; rowNo < rowCount; rowNo++) {
			
				var row = rowData[rowNo]
				var content = row[columnName]
				var cell = { content : content }
				
				if (row['.footnotes'] && row['.footnotes'][columnName])
					cell.footnotes = row['.footnotes'][columnName]
					
				if (colNo == 0 && columnDef.type == "string" && row[".rowLevel"])
					cell.content = Array(row[".rowLevel"]+1).join("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;") + cell.content
					
				if (row[".isNewGroup"]) {
				
					cell.isStartOfGroup = true
					isGrouped = true
				}
				
				if (isGrouped) {
				
					if (rowNo + 1 < rowCount) {
					
						if (rowData[rowNo + 1][".isNewGroup"])
							cell.isEndOfGroup = true
					}
					else {
					
						cell.isEndOfGroup = true					
					}
				}
				
				column[rowNo] = cell
			}
			
			columns[colNo] = column
		}

		var cells = Array(columnCount)
		
		for (var colNo = 0; colNo < columnCount; colNo++) {
		
			var column = columns[colNo]
			var name   = columnDefs[colNo].name
			var type   = columnDefs[colNo].type
			var format = columnDefs[colNo].format
			var alignNumbers = ! this.options.casesAcrossColumns  // numbers can't be aligned across rows
			var combine = columnDefs[colNo].combine
			
			cells[colNo] = this._formatColumn(column, type, format, alignNumbers, combine)
		}
		
		var columnsInColumn = { }  // dictionary of counts
		var columnsInsertedInColumn = { }
		var maxColumnsInColumn = 0
		var columnNames = [ ]
		
		for (var colNo = 0; colNo < columnCount; colNo++) {
		
			var columnName = this.options.schema.fields[colNo].name
			var subRowPos = columnName.indexOf("[")
			
			if (subRowPos != -1)
				columnName = columnName.substr(0, subRowPos)
			
			columnNames[colNo] = columnName
			
			var cic = columnsInColumn[columnName]
			
			if (typeof cic == "undefined")
				cic = 1
			else
				cic++
				
			if (maxColumnsInColumn < cic)
				maxColumnsInColumn = cic
			
			columnsInColumn[columnName] = cic
		}
		
		if (maxColumnsInColumn > 1) {  // do columns need to be folded
		
			var foldedColumnNames = _.uniq(columnNames)
			var foldedCells = Array(foldedColumnNames.length)
			var foldedColumnHeaders = Array(foldedColumnNames.length)
			
			// fold the headers

			for (var colNo = 0; colNo < foldedColumnNames.length; colNo++) {
			
				var headerIndex = columnNames.indexOf(foldedColumnNames[colNo])
				foldedColumnHeaders[colNo] = columnHeaders[headerIndex]
			}
			
			
			// fold the columns
			
			for (var colNo = 0; colNo < columnNames.length; colNo++) {
			
				var columnCells = cells[colNo]
				var columnName  = columnNames[colNo]
				var targetIndex = foldedColumnNames.indexOf(columnName)
				var column = foldedCells[targetIndex]
				var cic = columnsInColumn[columnName]
				
				if (typeof column == "undefined")
					column = Array(columnCells.length * cic)
					
				var offset = columnsInsertedInColumn[columnName]
				if (typeof offset == "undefined")
					offset = 0

				for (var rowNo = 0; rowNo < columnCells.length; rowNo++) {
				
					var cell = columnCells[rowNo]
					
					if (offset == 0)
						cell.isStartOfGroup = true
					if (offset == cic - 1)
						cell.isEndOfGroup = true
					
					cell.span = maxColumnsInColumn / cic
					column[rowNo * cic + offset] = cell
				
				}
				
				columnsInsertedInColumn[columnName] = offset + 1
				foldedCells[targetIndex] = column
			}
			
			cells = foldedCells
			columnHeaders = foldedColumnHeaders
			columnCount = foldedColumnHeaders.length
			rowCount *= maxColumnsInColumn
		}
		
		if (this.options.casesAcrossColumns) {
		
			var swapped = this._swapRowsAndColumns(columnHeaders, cells)
			cells = swapped.columns
			columnHeaders = swapped.columnHeaders;
			rowCount = swapped.rowCount
			columnCount = swapped.columnCount
		}
		
		var chunks = [ ]		

		if (this.options.error) {
		
			chunks.push('<table class="error-state">')
		}
		else {
		
			chunks.push('<table>')
		}
		
	

			chunks.push('<thead>')
				chunks.push('<tr>')
					chunks.push('<th colspan="' + 2 * columnCount + '">' + this.options.title + '<div class="toolbar do-not-copy"><div class="status"></div><div class="copy toolbar-button" style="visibility: hidden ;"></div><div class="cite toolbar-button" style="visibility: hidden ;"></div></div>')
					
					if (this.options.error && this.options.error.errorMessage) {
		
						chunks.push('<div  class="error-message-positioner">')
						chunks.push('<div  class="error-message-box ui-state-error">')
						chunks.push('<span class="error-message-symbol ui-icon ui-icon-alert"></span>')
						chunks.push('<div  class="error-message-message">' + this.options.error.errorMessage + '</div>')
						chunks.push('</div>')
						chunks.push('</div>')
					}
					
					chunks.push('</th>')
				chunks.push('</tr>')

		if (this.options.subtitle) {
				chunks.push('<tr>')
					chunks.push('<th colspan="' + 2 * columnCount + '"></th>')
				chunks.push('</tr>')
		}

				chunks.push('<tr>')

				
		for (var colNo = 0; colNo < columnHeaders.length; colNo++) {

			var cell = columnHeaders[colNo]
			
			var span = cell.span
			if (typeof span == "undefined")
				span = 1
				
			if (span) {

				span *= 2  // times 2, because of footnote markers
			
				chunks.push('<th colspan="' + span + '" class="' + cell.type + '">' + cell.content)
				if (cell.footnotes)
					chunks.push(cell.footnotes.join(' '))
				chunks.push('</th>')
			}

		}
				
				chunks.push('</tr>')
			chunks.push('</thead>')
			chunks.push('<tbody>')
			
		var tableProgress = Array(columnCount)
		for (var i = 0; i < columnCount; i++) {
		
			tableProgress[i] = { from : 0, to : 0 }
		}
		
		for (var rowNo = 0; rowNo < rowCount; rowNo++) {
			
			chunks.push('<tr>')

			for (var colNo = 0; colNo < columnCount; colNo++) {
			
				if (tableProgress[colNo].to == rowNo) {

					var fromIndex = tableProgress[colNo].from
					var cell = cells[colNo][fromIndex]
					var cellHtml = ''

					var cellClass = cell.class
					cellClass += (cell.isStartOfGroup ? " new-group-row" : "")
					cellClass += (cell.isEndOfGroup ? " last-group-row" : "")

					cellHtml += (cell.header ? '<th' : '<td')
					cellHtml += ' class="value ' + cellClass + '"'
					cellHtml += (cell.span   ? ' rowspan="' + cell.span + '"' : '')
					cellHtml += '>'
					cellHtml += (typeof cell.content   != "undefined" ? cell.content : '')
					cellHtml += (cell.header ? '</th>' : '</td>')
					
					cellHtml += (cell.header ? '<th' : '<td')
					cellHtml += ' class="symbol ' + cellClass + '"'
					cellHtml += (cell.span   ? ' rowspan="' + cell.span + '"' : '')
					cellHtml += '>'
					if (typeof cell.footnotes != "undefined")
						cellHtml += cell.footnotes.join(' ')
					cellHtml += (cell.header ? '</th>' : '</td>')
					
					tableProgress[colNo].from += 1
					
					if (cell.span) {
					
						tableProgress[colNo].to += cell.span

					}
					else {
					
						tableProgress[colNo].to += 1
					}
				
					chunks.push(cellHtml)
				}
				
			}
			
			if (rowNo == 0) // squashes the table to the left
				chunks.push('<td class="squash-left" rowspan="' + rowCount + '"></td>')

			chunks.push('</tr>')
		}
		
			chunks.push('<tr><td colspan="' + 2 * columnCount + '"></td></tr>')
	
			chunks.push('</tbody>')
			
		if (this.options.footnotes) {

			chunks.push('<tfoot>')

			for (var i = 0; i < this.options.footnotes.length; i++) {

				chunks.push('<tr><td colspan="' + 2 * columnCount + '">')
				
				var footnote = this.options.footnotes[i]
				
				if (_.isString(footnote)) {

					chunks.push(this._symbol(i) + '&nbsp;')
					chunks.push(footnote)
				}
				
				if (_.has(footnote, "symbol")) {
				
					if (_.isNumber(footnote.symbol))
						chunks.push(this._symbol(footnote.symbol) + '&nbsp;')
					else
						chunks.push(footnote.symbol + '&nbsp;')
						
					chunks.push(footnote.text)
				}
				
				chunks.push('</td></tr>')
			}

			chunks.push('</tfoot>')
		}

		chunks.push('</table>')
		
	
		var html = chunks.join("")

		this.element.html(html)
		
		var $table = this.element.children("table")
		var $toolbar = this.element.find("div.toolbar")
		
		var $toolbarButtons = $toolbar.find("div.toolbar-button")
		var $copy = $toolbar.find("div.copy")
		var $cite = $toolbar.find("div.cite")
		
		var $status = $toolbar.find("div.status")
		
		$table.mouseenter(function(event){
			$toolbarButtons.css("visibility", "visible")
		})
		$table.mouseleave(function(event) {
			$toolbarButtons.css("visibility", "hidden")
		})
		
		$copy.tooltip({ content: "Copied to clipboard", items: "*", disabled: true, show: { duration : 100 }, close : function() { window.setTimeout(function(){ $copy.tooltip("option", "disabled", true) }, 500) }, position: { my: "center+10 top+15", at: "center bottom", collision: "flipfit" }})
		$cite.tooltip({ content: "Citation copied to clipboard", items: "*", disabled: true, show: { duration : 100 }, close : function() { window.setTimeout(function(){ $cite.tooltip("option", "disabled", true) }, 500) }, position: { my: "center+10 top+15", at: "center bottom", collision: "flipfit" }})
		
		$copy.click(function(event) {
			pushToClipboard($table)
			event.preventDefault()
			$copy.tooltip("option", "disabled", false)
			$copy.tooltip("open")
			
			window.setTimeout(function() {
				$copy.tooltip("close")
			}, 800)
		})
		
		var citation = this.options.citation
		if (citation == null)
			$cite.hide()

		$cite.click(function(event) {
			pushTextToClipboard(citation.join("\n\n"))
			event.preventDefault()
			$cite.tooltip("option", "disabled", false)
			$cite.tooltip("open")
			
			window.setTimeout(function() {
				$cite.tooltip("close")
			}, 800)
		})
		
		$status.addClass(this.options.status)
		
	},
	_destroy: function () {
		this.element.removeClass("jasp-table").text("")
	}
})
