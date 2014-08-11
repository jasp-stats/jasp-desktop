$.widget("jasp.table", {

	options: {
		title: "",
		subtitle: null,
		variables: [ ],
		data: [ ],
		casesAcrossColumns : false,
		formats : null,
		status : "waiting"
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
	_formatColumn : function(column, type, format, alignNumbers) {

		var columnCells = Array(column.length)

		if (type == "string" || typeof format == "undefined") {
		
			for (var rowNo = 0; rowNo < column.length; rowNo++) {

				var clazz = (type == "string") ? "text" : "number"
				
				var cell = column[rowNo]
				if (typeof cell == "undefined")
					cell = "."
					
				columnCells[rowNo] = { content : cell, "class" : clazz }
			}
			
			return columnCells
		}

		var formats = format.split(";");
		var p = NaN
		var dp = NaN
		var sf = NaN

		for (var i = 0; i < formats.length; i++) {
		
			var f = formats[i]
			
			if (f.indexOf("p:") != -1)
			{
				p = f.substring(2)
			}
			if (f.indexOf("dp:") != -1) {

				dp = f.substring(3)
			}
			if (f.indexOf("sf:") != -1) {

				sf = f.substring(3)
			}
		}
		
		if (isFinite(sf)) {

			var lowerLim = -5
			var upperLim =  5
			var scientific = false
			var minLSD = Infinity	// right most position of the least significant digit
			var maxFSDOE = -Infinity  // left most position of the least significant digit of the exponent in scientific notation
			
			for (var rowNo = 0; rowNo < column.length; rowNo++) {

				var cell = column[rowNo]
				
				if (isNaN(parseFloat(cell)))  // isn't a number
					continue

				var fsd   = this._fsd(cell)  // position of first significant digit
				var lsd   = fsd - sf
				var fsdoe = this._fsdoe(cell)
				
				if (fsd >= upperLim || lsd <= lowerLim) {
				
					scientific = true
				}
				else if (lsd < minLSD) {
				
					minLSD = lsd
				}
				
				if (fsdoe > maxFSDOE)
					maxFSDOE = fsdoe
			}
		
			for (var rowNo = 0; rowNo < column.length; rowNo++) {

				var cell = column[rowNo]
				
				if (typeof cell == "undefined") {
				
					columnCells[rowNo] = { content : ".", "class" : "missing" }
				}
				else if (isNaN(parseFloat(cell))) {  // isn't a number
					
					columnCells[rowNo] = { content : cell, "class" : "number" }

				}
				else if (cell < p) {
					
					columnCells[rowNo] = { content : "<&nbsp" + p, "class" : "p-value" }

				}
				else if (scientific === false) {
				
					var fsd = this._fsd(cell)  // position of first significant digit
					var lsd = fsd - sf
					
					var paddingNeeded = Math.max(lsd - minLSD, 0)
					var padding = ""
					
					if (paddingNeeded && alignNumbers) {
						
						var dot = lsd == 0 ? "." : ""
						padding = '<span class="do-not-copy" style="visibility: hidden;">' + dot + Array(paddingNeeded + 1).join("0") + '</span>'
					}
					
					columnCells[rowNo] = { content : cell.toPrecision(sf).replace(/-/g, "&minus;") + padding, "class" : "number" }
				}
				else {
				
					var exponentiated = cell.toExponential(sf-1).replace(/-/g, "&minus;")
					var paddingNeeded = Math.max(maxFSDOE - this._fsdoe(cell), 0)
					
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
				
					columnCells[rowNo] = { content : reassembled, "class" : "number" }
				}
			}
		}
		else if (isFinite(dp)) {
		
			for (var rowNo = 0; rowNo < column.length; rowNo++) {

				var cell = column[rowNo]
				
				if (typeof cell == "undefined") {
				
					columnCells[rowNo] = { content : ".", "class" : "missing" }
				}
				else if (isNaN(parseFloat(cell))) {  // isn't a number
					
					columnCells[rowNo] = { content : cell, "class" : "number" }
				}
				else if (cell < p) {
					
					columnCells[rowNo] = { content : "<&nbsp" + p, "class" : "p-value" }
				}
				else {
				
					columnCells[rowNo] = { content : cell.toFixed(dp).replace(/-/g, "&minus;"), "class" : "number" }
				}
			}
		}
		else {
		
			for (var rowNo = 0; rowNo < column.length; rowNo++) {
			
				var cell = column[rowNo]
				if (typeof cell == "undefined")
					cell = "."
					
				columnCells[rowNo] = { content : cell }
			}
		}
		
		return columnCells
	
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
			var title = (typeof columnDef.title != "undefined") ? columnDef.title : columnDef.name
			
			columnHeaders[colNo] = { content : title, header : true }
			
			
			// populate cells column-wise
			
			var column = Array(rowCount)
			
			for (var rowNo = 0; rowNo < rowCount; rowNo++) {
			
				column[rowNo] = rowData[rowNo][columnDef.name]
			}
			
			columns[colNo] = column
		}

		var cells = Array(columnCount)
		
		for (var colNo = 0; colNo < columnCount; colNo++) {
		
			var column = columns[colNo]
			var type   = columnDefs[colNo].type
			var format = columnDefs[colNo].format
			var alignNumbers = ! this.options.casesAcrossColumns  // numbers can't be aligned across rows
			
			cells[colNo] = this._formatColumn(column, type, format, alignNumbers)
		}
		
		var foldedRows = false
		var columnsInColumn = { }  // dictionary of counts
		var columnsInsertedInColumn = { }
		var maxColumnsInColumn = 0
		var columnNames = [ ]
		
		for (var colNo = 0; colNo < columnCount; colNo++) {
		
			var columnName = this.options.schema.fields[colNo].name
			var subRowPos = columnName.indexOf("[")
			
			if (subRowPos != -1) {
			
				foldedRows = true
				columnName = columnName.substr(0, subRowPos)
			}
			
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
		
		if (foldedRows) {
		
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
					chunks.push('<th nowrap colspan="' + columnCount + '">' + this.options.title + '<div class="toolbar do-not-copy"><div class="copy" style="visibility: hidden ;"></div><div class="status"></div></div></th>')
				chunks.push('</tr>')

		if (this.options.subtitle) {
				chunks.push('<tr>')
					chunks.push('<th nowrap colspan="' + columnCount + '"></th>')
				chunks.push('</tr>')
		}

				chunks.push('<tr>')

				
		for (var colNo = 0; colNo < columnHeaders.length; colNo++) {

			var cell = columnHeaders[colNo]
			chunks.push('<th nowrap>' + cell.content + '</th>')

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

			var isMainRow = false

			for (var colNo = 0; colNo < columnCount; colNo++) {
			
				if (tableProgress[colNo].to == rowNo) {

					var fromIndex = tableProgress[colNo].from
					var cell = cells[colNo][fromIndex]
					var cellHtml = ''

					var cellClass = (cell.class ? cell.class + " " : "")
					cellClass += (isMainRow ? "main-row" : "")
				
					cellHtml += (cell.header ? '<th nowrap' : '<td nowrap')
					cellHtml += (cellClass   ? ' class="'   + cellClass + '"' : '')
					cellHtml += (cell.span   ? ' rowspan="' + cell.span + '"' : '')
					cellHtml += '>'
					cellHtml += (typeof cell.content != "undefined" ? cell.content : '')
					cellHtml += (cell.header  ? '</th>' : '</td>')
					
					tableProgress[colNo].from += 1
					
					if (cell.span) {
					
						tableProgress[colNo].to += cell.span
						
						if (cell.span > 1)
							isMainRow = true
					}
					else {
					
						tableProgress[colNo].to += 1
					}
				
					chunks.push(cellHtml)
				}
				
			}

			chunks.push('</tr>')
		}
		
			chunks.push('<tr><td colspan="' + columnCount + '"></td></tr>')
	
			chunks.push('</tbody>')
			
		if (this.options.footnotes) {

			chunks.push('<tfoot>')

			for (var i = 0; i < this.options.footnotes.length; i++) {

				chunks.push('<tr><td colspan="' + this.options.schema.fields.length + '">')
				
				var footnote = this.options.footnotes[i]
				
				if (_.isString(footnote)) {

					chunks.push('<sup>' + String.fromCharCode(97 + i) + '</sup>')
					chunks.push(footnote)
				}
				
				if (_.has(footnote, "symbol")) {
				
					chunks.push('<sup>' + footnote.symbol + '</sup>')
					chunks.push(footnote.text)
				}
				
				chunks.push('</td></tr>')
			}

			chunks.push('</tfoot>')
		}

		chunks.push('</table>')
		
		if (this.options.error && this.options.error.errorMessage) {
		
			chunks.push('<div style="position: absolute ; left: -1000 ; top: -1000 ;" class="error-message-box ui-state-error"><span class="ui-icon ui-icon-alert" style="float: left; margin-right: .3em;"></span>')
			chunks.push(this.options.error.errorMessage)
			chunks.push('</div>')
		}
		
		var html = chunks.join("")

		this.element.html(html)
		
		var $table = this.element.children("table")
		var $toolbar = this.element.find("div.toolbar")
		var $copy = $toolbar.find("div.copy")
		var $status = $toolbar.find("div.status")
		
		$table.mouseenter(function(event){
			$copy.css("visibility", "visible")
		})
		$table.mouseleave(function(event) {
			$copy.css("visibility", "hidden")
		})
		
		$copy.click(function(event) {
			pushToClipboard($table)
			event.preventDefault()
		})
		
		$status.addClass(this.options.status)
		
		if (this.options.error && this.options.error.errorMessage) {
			
			var $error = this.element.children("div.error-message-box")

			setTimeout(function() {
			
				var tablePos = $table.offset()
				var left = tablePos.left + ($table.width()  - $error.width()) / 2
				var top  = tablePos.top  + ($table.height() - $error.height()) / 2
				
				$error.offset({ top : top, left : left })
			
			}, 0)
		}
		
	},
	_destroy: function () {
		this.element.removeClass("jasp-table").text("")
	}
})
