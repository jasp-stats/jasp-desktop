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
	_formatColumn : function(column, type, format) {

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
			var minLsd = Infinity	// right most position of the least significant digit
			
			for (var rowNo = 0; rowNo < column.length; rowNo++) {

				var cell = column[rowNo]
				
				if (isNaN(parseFloat(cell)))  // isn't a number
					continue

				var fsd = this._fsd(cell)  // position of first significant digit
				var lsd = fsd - sf
				
				if (fsd > upperLim || lsd < lowerLim) {
				
					scientific = true
					break
				}
				else if (lsd < minLsd) {
				
					minLsd = lsd
				}
			}
		
			for (var rowNo = 0; rowNo < column.length; rowNo++) {

				var cell = column[rowNo]
				
				if (typeof cell == "undefined") {
				
					columnCells[rowNo] = { content : ".", "class" : "missing" }
				}
				else if (isNaN(parseFloat(cell))) {  // isn't a number
					
					columnCells[rowNo] = { content : cell, "class" : "text" }

				}
				else if (cell < p) {
					
					columnCells[rowNo] = { content : "<&nbsp" + p, "class" : "p-value" }

				}
				else if (scientific === false) {
				
					var fsd = this._fsd(cell)  // position of first significant digit
					var lsd = fsd - sf
					
					var paddingNeeded = Math.max(lsd - minLsd, 0)
					var padding = ""
					
					if (paddingNeeded)
						padding = '<span class="do-not-copy" style="visibility: hidden;">' + Array(paddingNeeded + 1).join("0") + '</span>'
					
					columnCells[rowNo] = { content : cell.toPrecision(sf) + padding, "class" : "number" }
				}
				else {
				
					columnCells[rowNo] = { content : cell.toExponential(sf-1), "class" : "number" }
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
					
					columnCells[rowNo] = { content : cell, "class" : "text" }
				}
				else if (cell < p) {
					
					columnCells[rowNo] = { content : "<&nbsp" + p, "class" : "p-value" }
				}
				else {
				
					columnCells[rowNo] = { content : cell.toFixed(dp), "class" : "number" }
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
			
			cells[colNo] = this._formatColumn(column, type, format)
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
					chunks.push('<th colspan="' + columnCount + '">' + this.options.title + '<div class="toolbar do-not-copy"><div class="copy" style="visibility: hidden ;"></div><div class="status"></div></div></th>')
				chunks.push('</tr>')

		if (this.options.subtitle) {
				chunks.push('<tr>')
					chunks.push('<th colspan="' + columnCount + '"></th>')
				chunks.push('</tr>')
		}

				chunks.push('<tr>')

				
		for (var colNo = 0; colNo < columnCount; colNo++) {

			var cell = columnHeaders[colNo]
			chunks.push('<th>' + cell.content + '</th>')

		}
				
				chunks.push('</tr>')
			chunks.push('</thead>')
			chunks.push('<tbody>')
		
		for (var rowNo = 0; rowNo < rowCount; rowNo++) {
			
			chunks.push('<tr>')

			for (var colNo = 0; colNo < columnCount; colNo++) {
			
				var cell = cells[colNo][rowNo]
				
				var cellHtml = ''
				
				cellHtml += (cell.header  ? '<th' : '<td')
				cellHtml += (cell.class   ? ' class="' + cell.class + '"' : '')
				cellHtml += '>'
				cellHtml += (cell.content ? cell.content : '')
				cellHtml += (cell.header  ? '</th>' : '</td>')
				
				chunks.push(cellHtml)
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
