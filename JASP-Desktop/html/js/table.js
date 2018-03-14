JASPWidgets.table = Backbone.Model.extend({

	defaults: {
		title: "",
		subtitle: null,
		variables: [],
		data: [],
		casesAcrossColumns: false,
		overTitle: false,
		formats: null,
		footnotes: [],
		citation: null,
		error: null,
		latexCode: ""
	}
});

JASPWidgets.tableView = JASPWidgets.objectView.extend({

	attachToolbar: function ($toolbar) {
		this.$el.addClass('jasp-display-item-flat');

		$toolbar.append('<div class="status"></div>');

		var $container = this.$el.find("div.toolbar");
		$container.append($toolbar);

		var optStatus = this.model.get("status");
		var $status = $toolbar.find("div.status");
		$status.addClass(optStatus);
	},

	copyMenuClicked: function () {
		var exportParams = new JASPWidgets.Exporter.params();
		exportParams.format = JASPWidgets.ExportProperties.format.html;
		exportParams.process = JASPWidgets.ExportProperties.process.copy;
		exportParams.htmlImageFormat = JASPWidgets.ExportProperties.htmlImageFormat.temporary;
		exportParams.includeNotes = false;

		this.exportBegin(exportParams);
		return true;
	},

	hasCitation: function () {
		var optCitation = this.model.get("citation");
		return optCitation !== null
	},

	citeMenuClicked: function () {
		var exportParams = new JASPWidgets.Exporter.params();
		exportParams.format = JASPWidgets.ExportProperties.format.html;
		exportParams.process = JASPWidgets.ExportProperties.process.copy;
		exportParams.htmlImageFormat = JASPWidgets.ExportProperties.htmlImageFormat.temporary;
		exportParams.includeNotes = false;

		var optCitation = this.model.get("citation");

		var htmlCite = '<p>' + optCitation.join("</p><p>") + '</p>';

		var exportContent = new JASPWidgets.Exporter.data(optCitation.join("\n\n"), htmlCite);

		pushTextToClipboard(exportContent, exportParams);
		return true;
	},

	hasLatexCode: function () {
		var optLatexCode = this.model.get("latexCode");
		return optLatexCode !== null
	},

	_generateLatexCode: function() {
		/**
		 * Generates the latex code for tables
		 */
		let optSchema = this.model.get("schema");
		let optData = this.model.get("data");
		let optTitle = this.model.get("title");
		let optCasesAcrossColumns = this.model.get("casesAcrossColumns");
		let optSubtitle = this.model.get("subtitle");
		let optVariables = this.model.get("variables");
		let optOverTitle = this.model.get("overTitle")
		let optFootnotes = this.model.get("footnotes");

		let columnsDict = createColumns(optSchema.fields, optData);
		let columnHeaders = columnsDict['columnHeaders'];
		let columns = columnsDict['columns'];

		let columnDefs = optSchema.fields;
		let columnCount = columnDefs.length;

		let rowData = optData;
		let rowCount = rowData ? rowData.length : 0;

		let cells = Array(columnCount);

		for (let colNo = 0; colNo < columnCount; colNo++) {

			let column = columns[colNo];
			let name = columnDefs[colNo].name;
			let type = columnDefs[colNo].type;
			let format = columnDefs[colNo].format;
			let alignNumbers = !optCasesAcrossColumns;  // numbers can't be aligned across rows
			let combine = columnDefs[colNo].combine;

			cells[colNo] = formatColumn(column, type, format, alignNumbers, combine, false);
		}

		var columnsInColumn = {}  // dictionary of counts
		var columnsInsertedInColumn = {}
		var maxColumnsInColumn = 0
		var columnNames = []

		for (var colNo = 0; colNo < columnCount; colNo++) {

			var columnName = optSchema.fields[colNo].name
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
				var columnName = columnNames[colNo]
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

		if (optCasesAcrossColumns) {

			var swapped = swapRowsAndColumns(columnHeaders, cells, optOverTitle)
			cells = swapped.columns
			columnHeaders = swapped.columnHeaders;
			rowCount = swapped.rowCount
			columnCount = swapped.columnCount
		}

		console.log("----------------");
		console.log(cells);
		console.log(columnHeaders);
		console.log("****************");

		return {'headers': columnHeaders, 'cells': cells}
	},

	_getLatexCode: function() {
		/**
		 * Generates the latex code for the table
		 */

		let optSchema = this.model.get("schema");
		let optData = this.model.get("data");
		let optTitle = this.model.get("title");
		let optSubtitle = this.model.get("subtitle");
		let optVariables = this.model.get("variables");
		let optOverTitle = this.model.get("overTitle")
		let optFootnotes = this.model.get("footnotes");

		let data = this._generateLatexCode();

		// TODO:
		//       2. handle all cases for tables
		//       3. caption and footnotes formatting

		let variable = "";  // required to find out the first column

		for (let i = 0; i < optSchema.fields.length; ++i) {
			if (optSchema.fields[i].title === "") {
				variable = optSchema.fields[i].name;
				break;
			}
		}

		// console.log(variable);

		let latexCode = "";
		// title
		latexCode += "\\begin{table}[h]\n\t\\caption{" + optTitle + "}\n";

		// alignments - {lrrr}
		let columns = data.headers.length;
		let alignments = '';

		for (let col = 0 ; col < columns; ++col) {

			// FIXME: 1. does not handle over title
			if (col == 0 || data.headers[col].content === undefined || data.headers[col].content === '') {
				alignments += 'l';
			} else {
				alignments += 'r';
			}
		}

		latexCode += "\t\\begin{tabular}{" + alignments + "}\n\t\t\\hline\n\t\t";

		for (let i = 0; i < columns; ++i) {
			latexCode += (formatCellforLatex(data.headers[i].content) + ' ');
			if (i !== columns - 1) {
				latexCode += '& ';
			}
		}
		latexCode += ' \\\\\n\t\t\\hline\n';

		// Find number of rows
		let maxRows = 0;
		for (let i = 0; i < columns ; ++i) {
			let rows = data.cells[i].length;
			if (rows > maxRows) {
				maxRows = rows;
			}
		}

		let firstColRow = 0;
		let incrementFirstCol = true;
		for (let r = 0; r < maxRows; ++r) {

			latexCode += '\t\t';
			if (incrementFirstCol) {
				latexCode += (formatCellforLatex(data.cells[0][firstColRow].content));
			}
			latexCode += (' & ');

			let isStartOfGroup = data.cells[0][firstColRow]['isStartOfGroup'];
			incrementFirstCol = !isStartOfGroup;

			if (isStartOfGroup === undefined || isStartOfGroup === false) {
				incrementFirstCol = true;
			} else {
				incrementFirstCol = false;
			}

			for (let c = 1; c < columns; ++c) {
				latexCode += (formatCellforLatex(data.cells[c][r].content) + ' ');
				if (c != columns - 1) {
					latexCode += ('& ');
				} else {
					latexCode += ' \\\\\n';
				}
			}

			if (!incrementFirstCol) {
				let isEndOfGroup = data.cells[columns - 1][r]['isEndOfGroup'];
				if (isEndOfGroup) {
					incrementFirstCol = true;
				}
			}

			if (incrementFirstCol) {
				firstColRow += 1;
			}
		}

		// for (let row = 0; row < optData.length; ++row) {
		// 	latexCode += '\t\t';
		//
		// 	let columns = optSchema.fields.length;
		//
		// 	for (let col = 0; col < columns; ++col) {
		//
		// 		// TODO: output should be formatted based on the value of "format" key
		// 		// TODO: subscripts and superscripts
		// 		// TODO: special characters - $, /, \, etc.
		// 		// TODO: Correlation matrix tables are a special case
		// 		// FIXME: convert NaN to ""
		//
		// 		let formattedText = optData[row][optSchema.fields[col].name];
		// 		if (optSchema.fields[col].type === "number") {
		// 			formattedText = Number(Number(formattedText).toPrecision(4)).toString();
		// 			// formattedText = Number(Number.parseFloat(Number(formattedText)).toPrecision(4)).toString();
		// 		}
		// 		latexCode += formattedText;
		//
		// 		if (col !== columns - 1) {
		// 			latexCode += " & ";
		// 		}
		// 	}
		// 	latexCode += ' \\\\\n';
		// }
		latexCode += '\t\t\\hline\n\t\\end{tabular} \n';

		if (optFootnotes !== "" && optFootnotes !== null && optFootnotes.length !== 0) {
			// latexCode += "\t{\\footnotesize \\textit{Note.} " + optFootnotes.join() + '\n';
		}

		latexCode += "\\end{table}"

		return latexCode;
	},

	latexCodeMenuClicked: function () {
		let exportParams = new JASPWidgets.Exporter.params();
		exportParams.format = JASPWidgets.ExportProperties.format.html;
		exportParams.process = JASPWidgets.ExportProperties.process.copy;
		exportParams.htmlImageFormat = JASPWidgets.ExportProperties.htmlImageFormat.temporary;
		exportParams.includeNotes = false;

		let latexCode = this._getLatexCode();

		let htmlCite = '<p>' + latexCode + '</p>';
		let exportContent = new JASPWidgets.Exporter.data(latexCode, htmlCite);
		pushTextToClipboard(exportContent, exportParams);

		return true;
	},

	indentChildren: false,

	menuName: "Table",

	events: {
		'mouseenter': '_hoveringStart',
		'mouseleave': '_hoveringEnd',
	},

	notePositionBottom: true,

	_hoveringStart: function (e) {
		this.toolbar.setVisibility(true);
	},

	_hoveringEnd: function (e) {
		this.toolbar.setVisibility(false);
	},

	hasCollapse: function () {
		return false;
	},

	constructChildren: function (constructor, data) {

		var self = this;
		this.toolbar.selectionElement = function () {
			return self.$el.find('th, td:not(.squash-left)');
		};

		var tablePrimative = new JASPWidgets.tablePrimative({ model: this.model, className: "jasp-table-primative jasp-display-primative" });
		this.localViews.push(tablePrimative);
		this.views.push(tablePrimative);
	},

	titleFormatOverride: 'span',

	disableTitleExport: true,
});

JASPWidgets.tablePrimative = JASPWidgets.View.extend({

	_fsd: function (value) { // first significant digit position

		if (value > 0)
			return Math.floor(Math.log(+value) / Math.log(10)) + 1
		else if (value < 0)
			return Math.floor(Math.log(-value) / Math.log(10)) + 1
		else
			return 1

	},

	_fsdoe: function (value) { // first significant digit position in the exponent in scientific notation

		if (value == 0)
			return 1

		var exponent = Math.floor(Math.log(Math.abs(value)) / Math.log(10))

		return this._fsd(exponent)

	},

	_symbol: function (index) {

		// no c in webkit?!

		return ("\u1D43\u1D47" /* \u1D9C */ + "\u1D48\u1D49\u1DA0\u1D4D\u02B0\u2071\u02B2\u1D4F\u02E1\u1D50\u207F\u1D52\u1D56\u02B3\u02E2\u1D57\u1D58\u1D5B\u02B7\u02E3\u02B8\u1DBB").charAt(index)

	},

	_swapRowsAndColumns: function (columnHeaders, columns, optOverTitle) {

		var newRowCount = columns.length - 1
		var newColumnCount = columns[0].length + 1

		if (optOverTitle) {

			// Transform first column into overtitle, second into title
			var newColumnHeaders = Array(newColumnCount-1);
			for (var colNo = 0; colNo < newColumnCount - 1; colNo++) {
				newColumnHeaders[colNo] = { content: columns[1][colNo].content,
																		header: true,
																		overTitle: columns[0][colNo].content,
																		type: "string" };
			}
			// remove the column that became title
			columns.shift();
			columnHeaders.shift();
			newRowCount--;

		} else {

			var newColumnHeaders = Array(newColumnCount);
			var newColumnHeaders = columns[0];

		}

		var newColumns = Array(newColumnCount)

		var cornerCell = columnHeaders.shift()
		newColumnHeaders.unshift(cornerCell)
		newColumns[0] = columnHeaders

		for (var colNo = 1; colNo < newColumnCount; colNo++) {

			newColumns[colNo] = Array(newRowCount)

			for (var rowNo = 0; rowNo < newRowCount; rowNo++) {

				newColumns[colNo][rowNo] = columns[rowNo + 1][colNo - 1]
			}

		}

		return { columnHeaders: newColumnHeaders, columns: newColumns, rowCount: newRowCount, columnCount: newColumnCount }

	},

	_formatColumn: function (column, type, format, alignNumbers, combine, html = true) {

		var columnCells = Array(column.length)

		if (type == "string" || typeof format == "undefined") {

			for (var rowNo = 0; rowNo < column.length; rowNo++) {

				var clazz = (type == "string") ? "text" : "number"

				var cell = column[rowNo]
				var content = cell.content
				var formatted
				var combined = false

				if (typeof content == "undefined") {

					formatted = { content: "." }
				}
				else if (combine && rowNo > 0 && column[rowNo - 1].content == content) {
                    clazz += " combined";

					let content = "&nbsp;";

					if (!html) {
						content = " ";
					}

					formatted = { content: content, class: clazz }
					combined = true
				}
				else {

					if (typeof content === "string") {
						if (html) {
							content = content.replace(/\u273B/g, "<small>\u273B</small>")
						}
					}

					formatted = { content: content, "class": clazz }
				}

				if (combined == false && cell.isStartOfGroup)
					formatted.isStartOfGroup = true

				if (cell.isStartOfSubGroup)
					formatted.isStartOfSubGroup = true

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
		var log10 = false
		var dp = parseInt(window.globSet.decimals); // NaN if not specified by user
		var fixDecimals = (typeof dp === 'number') && (dp % 1 === 0);

		for (var i = 0; i < formats.length; i++) {

			var f = formats[i]

			if (f.indexOf("p:") != -1) {
				// override APA style if exact p-values wanted
				if (window.globSet.pExact){
					sf = 4;
				} else {
					p = f.substring(2);
				}
			}

			if (f.indexOf("dp:") != -1 && ! fixDecimals)
				dp = f.substring(3)

			if (f.indexOf("sf:") != -1)
				sf = f.substring(3)

			if (f.indexOf("pc") != -1)
				pc = true

			if (f.indexOf("~") != -1)
				approx = true;

			if (f.indexOf("log10") != -1)
				log10 = true
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

				var fsd  // position of first significant digit

				if (log10)
					fsd = content
				else
					fsd = this._fsd(content)

				var lsd = fsd - sf

				if (log10) {

					if (content >= 6 || content <= -dp) {

						fsdoe = this._fsd(content)

						if (fsdoe > maxFSDOE)
							maxFSDOE = fsdoe
					}

				} else if (Math.abs(content) >= upperLimit || Math.abs(content) <= Math.pow(10, -dp)) {

					var fsdoe   // first significant digit of exponent

					fsdoe = this._fsdoe(content)

					if (fsdoe > maxFSDOE)
						maxFSDOE = fsdoe
				}

				if (lsd < minLSD) {

					minLSD = lsd
				}
			}

			if (fixDecimals) {
				minLSD = -dp
			} else {
				if (minLSD < -dp)
					minLSD = -dp
				if (minLSD > 0)
					minLSD = 0
			}

			if (minLSD < -20)
				minLSD = -20

			for (var rowNo = 0; rowNo < column.length; rowNo++) {

				var cell = column[rowNo]
				var content = cell.content
				var formatted
				var isNumber = false

				if (typeof content == "undefined") {

					formatted = { content: "." }
				}
				else if (typeof content === "") {
					let content = (html) ? "&nbsp;" : " ";
					formatted = { content: content, "class": "number" }
				}
				else if (combine && rowNo > 0 && column[rowNo - 1].content == content) {
					let content = (html) ? "&nbsp;" : " ";
					formatted = { content: content, "class": "number" }
				}
				else if (isNaN(parseFloat(content))) {  // isn't a number
					formatted = { content: content, "class": "number" }
				}
				else if (content < p) {
					let content = (html) ? "<&nbsp;" : "< ";
					formatted = { content: content + p, "class": "p-value" }
				}
				else if (content == 0) {

					var number = 0

					if (log10)
						number = 1

					if (isFinite(dp))
						formatted = { content: number.toFixed(dp), "class": "number" }
					else
						formatted = { content: number.toPrecision(sf), "class": "number" }

					isNumber = true
				}
				else if (log10) {

					if (content < (Math.log(upperLimit) / Math.log(10)) && content > -dp) {

						if (alignNumbers || fixDecimals) {
							let _sign = (html) ? "&minus;" : "-";
							formatted = { content: Math.pow(10, content).toFixed(-minLSD).replace(/-/g, _sign), "class": "number" }
						}
						else {
							let _sign = (html) ? "&minus;" : "-";
							formatted = { content: Math.pow(10, content).toPrecision(sf).replace(/-/g, _sign), "class": "number" }
						}

						isNumber = true
					}
					else {

						var paddingNeeded = Math.max(maxFSDOE - this._fsd(content), 0)

						var exponent = Math.abs(Math.floor(content))

						var exp = ""

						while (exponent > 0) {

							var digit = exponent % 10
							exponent = Math.floor(exponent / 10)
							exp = "" + digit + exp
						}

						if (exp.length === 0)
							exp = "1"

						exponent = exp

						var mantissa
						if (content > 0)
							mantissa = Math.pow(10, content % 1)
						else
							mantissa = Math.pow(10, 1 + (content % 1))

						if (mantissa > 9.99999999) {

							mantissa = 1
							exponent--
						}

						var sign = content >= 0 ? "+" : "-"

						mantissa = fixDecimals ? mantissa.toFixed(dp) : mantissa.toPrecision(sf)

						var padding

						if (paddingNeeded)
							padding = '<span class="do-not-copy" style="visibility: hidden;">' + Array(paddingNeeded + 1).join("0") + '</span>'
						else
							padding = ''

						let reassembled;

						if (html) {
							reassembled = mantissa + "e&thinsp;" + padding + sign + exponent;
						} else {
							reassembled = mantissa + "e" + sign + exponent;
						}

						formatted = { content: reassembled, "class": "number" }

						isNumber = true
					}
				}
				else if (Math.abs(content) >= upperLimit || Math.abs(content) <= Math.pow(10, -dp)) {

					var decimalsExpon = fixDecimals ? dp : sf - 1;
					let _sign = (html) ? "&minus;" : "-";
					var exponentiated = content.toExponential(decimalsExpon).replace(/-/g, _sign)
					var paddingNeeded = Math.max(maxFSDOE - this._fsdoe(content), 0)

					var split = exponentiated.split("e")
					var mantissa = split[0]
					var exponent = split[1]
					var exponentSign = exponent.substr(0, 1)
					var exponentNum = exponent.substr(1)

					var padding

					if (paddingNeeded)
						padding = '<span class="do-not-copy" style="visibility: hidden;">' + Array(paddingNeeded + 1).join("0") + '</span>'
					else
						padding = ''


					let reassembled;
					if (html) {
						reassembled = mantissa + "e&thinsp;" + padding + exponentSign + exponentNum;
					} else {
						reassembled = mantissa + "e" + exponentSign + exponentNum;
					}

					formatted = { content: reassembled, "class": "number" }

					isNumber = true
				}
				else {

					if (alignNumbers || fixDecimals) {
						let _sign = (html) ? "&minus;" : "-";
						formatted = { content: content.toFixed(-minLSD).replace(/-/g, _sign), "class": "number" }
					}
					else {
						let _sign = (html) ? "&minus;" : "-";
						formatted = { content: content.toPrecision(sf).replace(/-/g, _sign), "class": "number" }
					}

					isNumber = true
				}

				if (typeof cell.footnotes != "undefined")
					formatted.footnotes = this._getFootnotes(cell.footnotes)

				if (cell.isStartOfGroup)
					formatted["class"] += " new-group-row"

				if (cell.isStartOfSubGroup)
					formatted["class"] += " new-sub-group-row"

				if (cell.isEndOfGroup)
					formatted["class"] += " last-group-row"

				if (isNumber && approx) {
					let _content = (html) ? "~&thinsp;" : "~";
					formatted.content = _content + formatted.content
				}

				columnCells[rowNo] = formatted
			}
		}
		else if (isFinite(dp)) {

			for (var rowNo = 0; rowNo < column.length; rowNo++) {

				var cell = column[rowNo]
				var content = cell.content
				var formatted

				var isNumber = false

				if (typeof content == "undefined") {

					formatted = { content: "." }
				}
				else if (content === "") {
					let _content = (html) ? "&nbsp;" : " ";
					formatted = { content: _content }
				}
				else if (combine && rowNo > 0 && column[rowNo - 1].content == content) {
					let _content = (html) ? "&nbsp;" : " ";
					formatted = { content: _content, "class": "number" }
				}
				else if (isNaN(parseFloat(content))) {  // isn't a number
					formatted = { content: content, "class": "number" }
				}
				else if (content < p) {
					let _content = (html) ? "<&nbsp;" : "< ";
					formatted = { content: _content + p, "class": "p-value" }
				}
				else if (pc) {
					let _content = (html) ? "&thinsp;%" : "%";
					formatted = { content: "" + (100 * content).toFixed(dp) + _content, "class": "percentage" }
					isNumber = true
				}
				else {
					let _content = (html) ? "&minus;" : "-";
					formatted = { content: content.toFixed(dp).replace(/-/g, _content), "class": "number" }
					isNumber = true
				}

				if (typeof cell.footnotes != "undefined")
					formatted.footnotes = this._getFootnotes(cell.footnotes)

				if (cell.isStartOfGroup)
					formatted["class"] += " new-group-row"

				if (cell.isStartOfSubGroup)
					formatted["class"] += " new-sub-group-row"

				if (cell.isEndOfGroup)
					formatted["class"] += " last-group-row"

				if (isNumber && approx) {
					let _content = (html) ? "~&thinsp;" : "~";
					formatted.content = _content + formatted.content
				}

				columnCells[rowNo] = formatted
			}
		}
		else if (pc) {

			for (var rowNo = 0; rowNo < column.length; rowNo++) {

				var cell = column[rowNo]
				var content = cell.content
				var formatted

				var isNumber = false

				if (typeof content == "undefined") {

					formatted = { content: "." }
				}
				else if (content === "") {
					let _content = (html) ? "&nbsp" : " ";
					formatted = { content: _content }
				}
				else if (isNaN(parseFloat(content))) {  // isn't a number
					formatted = { content: content, "class": "percentage" }
				}
				else {
					let _content = (html) ? "&thinsp;%" : "%";
					formatted = { content: "" + (100 * content.toFixed(0)) + _content, "class": "percentage" }
					isNumber = true
				}

				if (typeof cell.footnotes != "undefined")
					formatted.footnotes = this._getFootnotes(cell.footnotes)

				if (cell.isStartOfGroup)
					formatted["class"] += " new-group-row"

				if (cell.isStartOfSubGroup)
					formatted["class"] += " new-sub-group-row"

				if (cell.isEndOfGroup)
					formatted["class"] += " last-group-row"

				if (isNumber && approx) {
					let _content = (html) ? "~&thinsp;" : "~";
					formatted.content = _content + formatted.content
				}

				columnCells[rowNo] = formatted
			}
		}
		else {

			for (var rowNo = 0; rowNo < column.length; rowNo++) {

				var cell = column[rowNo]
				var content = cell.content
				var formatted

				if (typeof content == "undefined") {

					formatted = { content: "." }
				}
				else if (content === "") {
					let _content = (html) ? "&nbsp;" : " ";
					formatted = { content: _content }
				}
				else if (combine && rowNo > 0 && column[rowNo - 1].content == content) {
					let _content = (html) ? "&nbsp;" : " ";
					formatted = { content: _content }
				}
				else {
					formatted = { content: content }
				}

				if (typeof cell.footnotes != "undefined")
					formatted.footnotes = this._getFootnotes(cell.footnotes)

				if (cell.isStartOfGroup)
					formatted["class"] += " new-group-row"

				if (cell.isStartOfSubGroup)
					formatted["class"] += " new-sub-group-row"

				if (cell.isEndOfGroup)
					formatted["class"] += " last-group-row"

				columnCells[rowNo] = formatted
			}
		}

		return columnCells

	},

	_getFootnotes: function (indices) {

		var footnotes = Array(indices.length)
		var optFootnotes = this.model.get("footnotes");

		for (var i = 0; i < indices.length; i++) {

			var index = indices[i]

			if (_.isString(index)) {

				footnotes[i] = index

			} else if (index < optFootnotes.length) {

				var footnote = optFootnotes[index]
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

	_createColumns: function(columnDefs, rowData) {
		/**
		 * Returns 'columns' data array
		 * @param columnDefs Schema (fields)
		 * @param rowData    Table data
		 */

		let columnCount = columnDefs.length;
		let columns = Array(columnCount);
		let columnHeaders = Array(columnCount);
		let rowCount = rowData ? rowData.length : 0;

		for (let colNo = 0; colNo < columnCount; colNo++) {

			// populate column headers
			let columnDef = columnDefs[colNo];
			let columnName = columnDef.name;

			let title = columnDef.title;
			let overTitle = columnDef.overTitle;

			if (typeof title == "undefined") {
				title = columnName;
			}

			if (title == "") {
				title = "&nbsp;";
			}

			let columnType = columnDef.type;
			let columnHeader = {content: title, header: true, type: columnType};

			if (overTitle) {
				columnHeader.overTitle = overTitle;
			}

			if (typeof columnDef[".footnotes"] != "undefined") {
				columnHeader.footnotes = this._getFootnotes(columnDef[".footnotes"]);
			}

			columnHeaders[colNo] = columnHeader;

			// populate cells column-wise
			let column = Array(rowCount);
			let isGrouped = false;

			for (let rowNo = 0; rowNo < rowCount; rowNo++) {

				let row = rowData[rowNo];
				let content = row[columnName];
				let cell = { content: content };

				if (row['.footnotes'] && row['.footnotes'][columnName])
					cell.footnotes = row['.footnotes'][columnName];

				if (colNo == 0 && columnDef.type == "string" && row[".rowLevel"])
					cell.content = Array(row[".rowLevel"] + 1).join("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;") + cell.content;

				if (row[".isNewGroup"]) {
					cell.isStartOfGroup = true;
					isGrouped = true;
				}

				if (row[".isNewSubGroup"]) {
					cell.isStartOfSubGroup = true;
					isGrouped = true;
				}

				if (isGrouped) {
					if (rowNo + 1 < rowCount) {
						if (rowData[rowNo + 1][".isNewGroup"])
							cell.isEndOfGroup = true;
					}
					else {
						cell.isEndOfGroup = true;
					}
				}
				column[rowNo] = cell;
			}

			columns[colNo] = column;
		}

		return {
			'columns': columns,
			'columnHeaders': columnHeaders
		};
	},

	render: function () {
		var optSchema = this.model.get("schema");
		var optData = this.model.get("data");
		var optTitle = this.model.get("title");
		var optSubtitle = this.model.get("subtitle");
		var optCasesAcrossColumns = this.model.get("casesAcrossColumns");
		var optOverTitle = this.model.get("overTitle")
		var optFootnotes = this.model.get("footnotes");
		var optCitation = this.model.get("citation");
		var optStatus = this.model.get("status");
		var optError = this.model.get("error");

		var columnDefs = optSchema.fields
		var columnCount = columnDefs.length

		let rowData = optData;
		let rowCount = rowData ? rowData.length : 0

		let columnsDict = this._createColumns(columnDefs, rowData);
		let columnHeaders = columnsDict['columnHeaders'];
		let columns = columnsDict['columns'];

		var cells = Array(columnCount);

		for (var colNo = 0; colNo < columnCount; colNo++) {

			var column = columns[colNo]
			var name = columnDefs[colNo].name
			var type = columnDefs[colNo].type
			var format = columnDefs[colNo].format
			var alignNumbers = !optCasesAcrossColumns  // numbers can't be aligned across rows
			var combine = columnDefs[colNo].combine

			cells[colNo] = this._formatColumn(column, type, format, alignNumbers, combine)
		}

		var columnsInColumn = {}  // dictionary of counts
		var columnsInsertedInColumn = {}
		var maxColumnsInColumn = 0
		var columnNames = []

		for (var colNo = 0; colNo < columnCount; colNo++) {

			var columnName = optSchema.fields[colNo].name
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
				var columnName = columnNames[colNo]
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

		if (optCasesAcrossColumns) {

			var swapped = this._swapRowsAndColumns(columnHeaders, cells, optOverTitle)
			cells = swapped.columns
			columnHeaders = swapped.columnHeaders;
			rowCount = swapped.rowCount
			columnCount = swapped.columnCount
		}

		var chunks = []

		if (optError) {

			chunks.push('<table class="error-state jasp-no-select">')
		}
		else {

			chunks.push('<table class="jasp-no-select">')
		}


		chunks.push('<thead>')
		chunks.push('<tr>')
		chunks.push('<th colspan="' + 2 * columnCount + '"><div class="toolbar"></div></div>')

		if (optError && optError.errorMessage) {

			chunks.push('<div  class="error-message-positioner">')
			chunks.push('<div  class="error-message-box ui-state-error">')
			chunks.push('<span class="error-message-symbol ui-icon ui-icon-alert"></span>')
			chunks.push('<div  class="error-message-message">' + optError.errorMessage + '</div>')
			chunks.push('</div>')
			chunks.push('</div>')
		}

		chunks.push('</th>')
		chunks.push('</tr>')

		if (optSubtitle) {
			chunks.push('<tr>')
			chunks.push('<th colspan="' + 2 * columnCount + '"></th>')
			chunks.push('</tr>')
		}

		if (columnHeaders.length > 0) {

			var overTitles = false;
			var overTitleSpace = false;
			var overTitlesArray = [];
			// Find the overTitles
			for (var i = 0; i < columnHeaders.length; i++) {
				if (typeof columnHeaders[i].overTitle != "undefined") {
					overTitles = true
					break;
				}
			}

			if (overTitlesArray.length > 0) {
				// If we have an overTitle, we should make it
				overTitles = true;
			}

			var uniqueOverTitles = $.unique(overTitlesArray)
			if (uniqueOverTitles.length > 1) {
				// If we have more than one unique overTitle, we should make small
				// breaks in the line under the overTitle to indicate end of old and
				// start of new overTitle. NB: with this option, the line is not copied
				// to text processor.
				overTitleSpace = true;
			}

			if (overTitles) {

				if (overTitleSpace) {
					chunks.push('<tr class="over-title-space">')
				} else {
					chunks.push('<tr class="over-title">')
				}


				var span = 1;
				var oldTitle = columnHeaders[0].overTitle
				var newTitle = ""

				if (!oldTitle)
					oldTitle = ""

				for (var colNo = 1; colNo < columnHeaders.length; colNo++) {

					newTitle = columnHeaders[colNo].overTitle
					if (!newTitle)
						newTitle = ""

					if (newTitle == oldTitle) {

						span++
					}
					else {
						if (overTitleSpace) {
							chunks.push('<th colspan="' + (2 * span) + '"><div class="over-title-space">' + oldTitle + '</div></th>');
						} else {
							chunks.push('<th colspan="' + (2 * span) + '">' + oldTitle + '</th>');
						}
						oldTitle = newTitle
						span = 1
					}
				}

				if (newTitle == oldTitle) {
					if (overTitleSpace) {
						chunks.push('<th colspan="' + (2 * span) + '"><div class="over-title-space">' + newTitle + '</div></th>')
					} else {
						chunks.push('<th colspan="' + (2 * span) + '">' + newTitle + '</th>')
					}
				}


				chunks.push('</tr>')
			}
		}

		chunks.push('<tr>')


		for (var colNo = 0; colNo < columnHeaders.length; colNo++) {

			var cell = columnHeaders[colNo]

			if (cell.content == "\u03B7\u00B2\u209A")
				cell.content = '&eta;&sup2;<small><sub style="position: relative; left: -1ex;">p</sub></small>'

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

			tableProgress[i] = { from: 0, to: 0 }
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
					cellClass += (cell.isStartOfSubGroup ? " new-sub-group-row" : "")
					cellClass += (cell.isEndOfGroup ? " last-group-row" : "")
                    cellClass += (cell.span > 1 ? " row-span" : "")

					cellHtml += (cell.header ? '<th' : '<td')
					cellHtml += ' class="value ' + cellClass + '"'
					cellHtml += (cell.span ? ' rowspan="' + cell.span + '"' : '')
					cellHtml += '>'
					cellHtml += (typeof cell.content != "undefined" ? cell.content : '')
					cellHtml += (cell.header ? '</th>' : '</td>')

					cellHtml += (cell.header ? '<th' : '<td')
					cellHtml += ' class="symbol ' + cellClass + '"'
					cellHtml += (cell.span ? ' rowspan="' + cell.span + '"' : '')
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

		if (optFootnotes) {

			chunks.push('<tfoot>')

			for (var i = 0; i < optFootnotes.length; i++) {

				chunks.push('<tr><td colspan="' + 2 * columnCount + '">')

				var footnote = optFootnotes[i]

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

			chunks.push('</tfoot>');
		}

		chunks.push('</table>');


		var html = chunks.join("");

		this.$el.append(html);
	},

	getExportAttributes: function (element, exportParams) {
		var attrs = ""
		var style = ""

		var $elObj = $(element)
		var tag = $elObj.prop("tagName").toLowerCase()

		if (tag === "td" || tag === "th") {

			style = JASPWidgets.Exporter.getTableContentStyles($elObj, exportParams);

			if ($elObj.prop("rowspan") && $elObj.prop("rowspan") != 1)
				attrs += 'rowspan="' + $elObj.prop("rowspan") + '" '
			if ($elObj.prop("colspan") && $elObj.prop("colspan") != 1)
				attrs += 'colspan="' + $elObj.prop("colspan") + '" '
		}
		else if (tag === "table") {
			style = JASPWidgets.Exporter.getTableStyles($elObj, exportParams);
		}
		else if (tag === "span" || tag === "h1" || tag === "h2" || tag === "h3") {
			style = JASPWidgets.Exporter.getHeaderStyles($elObj, exportParams);
		}
		else if ($elObj.is('.error-message-positioner')) {
			style = JASPWidgets.Exporter.getErrorStyles($elObj, 'error-message-positioner');
		}
		else if ($elObj.is('.error-message-box')) {
			style = JASPWidgets.Exporter.getErrorStyles($elObj, 'error-message-box');
		}
		else if ($elObj.is('.error-message-symbol')) {
			style = JASPWidgets.Exporter.getErrorStyles($elObj, 'error-message-symbol');
		}
		else if ($elObj.is('.error-message-message')) {
			style = JASPWidgets.Exporter.getErrorStyles($elObj, 'error-message-message');
		}


		if (style)
			attrs = style + ' ' + attrs

		return attrs;
	},

	exportHTML: function (exportParams, element, tabs) {
		if (element == null)
			element = this.$el;

		tabs = tabs || ""

		var text = ""
		var $elObj = $(element)

		if ($elObj.hasClass("do-not-copy") || $elObj.is("td.squash-left"))
			return text

		var tag = $elObj.prop("tagName").toLowerCase()

		var attrs = this.getExportAttributes(element, exportParams);

		if (attrs)
			text = tabs + '<' + tag + ' ' + attrs + '>'
		else
			text = tabs + '<' + tag + '>'

		var contents = $elObj.contents()

		if (contents.length > 0) {

			for (var i = 0; i < contents.length; i++) {
				var node = contents[i]
				if (node.nodeType === 3) { //is text node
					var value = $(node).text()
					if (value) {

						value = value
							.replace(/&/g, '&amp;')
							.replace(/"/g, '&quot;')
							.replace(/'/g, '&#39;')
							.replace(/</g, '&lt;')
							.replace(/>/g, '&gt;')
							.replace(/\u2212/g, '-')

						text += "\n" + tabs + value + "\n"
					}
				}
				else {
					text += "\n" + this.exportHTML(exportParams, contents[i], tabs + "\t");
				}
			}

			text += tabs + '</' + tag + '>\n'
		}
		else {

			text += '</' + tag + '>\n'
		}

		return text;
	},

	exportBegin: function (exportParams, completedCallback) {

		if (exportParams == undefined)
			exportParams = new JASPWidgets.Exporter.params();
		else if (exportParams.error)
			return false;

		var callback = this.exportComplete;
		if (completedCallback !== undefined)
			callback = completedCallback;

		if (exportParams.includeNotes && this.noteBox !== undefined && this.noteBox.visible && this.noteBox.isTextboxEmpty() === false) {
			var exportObject = {
				views: [this, this.noteBox],
				getStyleAttr: function () {
					return "style='display: block;'";
				}
			};
			var newParams = exportParams.clone();
			newParams.includeNotes = false;

			JASPWidgets.Exporter.begin(exportObject, newParams, callback, true);
		}
		else
			callback.call(this, exportParams, new JASPWidgets.Exporter.data(null, this.exportHTML(exportParams)));

		return true;
	},

	exportComplete: function (exportParams, exportContent) {
		if (!exportParams.error)
			pushHTMLToClipboard(exportContent, exportParams);
	},
});
