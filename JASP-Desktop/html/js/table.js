JASPWidgets.table = Backbone.Model.extend({

	defaults: {
		title:				"",
		subtitle:			null,
		variables:			[],
		data:				[],
		casesAcrossColumns:	false,
		overTitle:			false,
		formats:			null,
		footnotes:			[],
		citation:			null,
		error:				null,
		latexCode:			"",
		name:				"",
		showsStatus:		true
	}
});

JASPWidgets.tableView = JASPWidgets.objectView.extend({

	attachToolbar: function ($toolbar) {
		this.$el.addClass('jasp-display-item-flat');

		var $container = this.$el.find("div.toolbar"); // the table toolbar is nested within the table to ensure that the table header gets copied too
		$container.append($toolbar);
	},

	copyMenuClicked: function () {
		var exportParams				= new JASPWidgets.Exporter.params();
		exportParams.format				= JASPWidgets.ExportProperties.format.html;
		exportParams.process			= JASPWidgets.ExportProperties.process.copy;
		exportParams.htmlImageFormat	= JASPWidgets.ExportProperties.htmlImageFormat.temporary;
		exportParams.includeNotes		= false;

		this.exportBegin(exportParams);
		return true;
	},

	showDependenciesClicked: function() { this.model.trigger("ShowDependencies:clicked", this.model.get("name")); },

	hasLaTeXCode: function () {
	  var optLaTeXCode = this.model.get("latexCode");
	  return optLaTeXCode !== null
	},

	_generateLaTeXCode: function() {
		/**
		 * Generates the latex code for tables
		 */
		let optSchema = this.model.get("schema");
		let optData = this.model.get("data");
		let optCasesAcrossColumns = this.model.get("casesAcrossColumns");
		let optOvertitle = this.model.get("overTitle")
		let optFootnotes = this.model.get("footnotes");

		let columnsDict = createColumns(optSchema.fields, optData, optFootnotes);
		let columnHeaders = columnsDict['columnHeaders'];
		let columns = columnsDict['columns'];

		let columnDefs = optSchema.fields;
		let columnCount = columnDefs.length;

		let rowCount = optData.length > 1 ? optData.length : 1;
		let cells = Array(columnCount);

		for (let colNo = 0; colNo < columnCount; colNo++) {
			cells[colNo] = formatColumn(
				columns[colNo],
				columnDefs[colNo].type,
				columnDefs[colNo].format,
				!optCasesAcrossColumns,
				columnDefs[colNo].combine,
				optFootnotes,
				false
			);
		}

		let columnsInColumn = {};  // dictionary of counts
		let columnsInsertedInColumn = {};
		let maxColumnsInColumn = 0;
		let columnNames = [];

		for (let colNo = 0; colNo < columnCount; colNo++) {
			let columnName = optSchema.fields[colNo].name;
			let subRowPos = columnName.indexOf("[");

			if (subRowPos !== -1) {
				columnName = columnName.substr(0, subRowPos);
			}
			columnNames[colNo] = columnName;

			let cic = columnsInColumn[columnName];
			if (typeof cic == "undefined") {
				cic = 1;
			} else {
				cic++;
			}
			if (maxColumnsInColumn < cic) {
				maxColumnsInColumn = cic;
			}

			columnsInColumn[columnName] = cic;
		}

		if (maxColumnsInColumn > 1) {
			let foldedColumnNames = _.uniq(columnNames);
			let foldedCells = Array(foldedColumnNames.length);
			let foldedColumnHeaders = Array(foldedColumnNames.length);

			// fold the headers
			for (let colNo = 0; colNo < foldedColumnNames.length; colNo++) {
				let headerIndex = columnNames.indexOf(foldedColumnNames[colNo]);
				foldedColumnHeaders[colNo] = columnHeaders[headerIndex];
			}

			// fold the columns
			for (let colNo = 0; colNo < columnNames.length; colNo++) {

				let columnCells = cells[colNo];
				let columnName = columnNames[colNo];
				let targetIndex = foldedColumnNames.indexOf(columnName);
				let column = foldedCells[targetIndex];
				let cic = columnsInColumn[columnName];

				if (typeof column == "undefined") {
					column = Array(columnCells.length * cic);
				}

				let offset = columnsInsertedInColumn[columnName];
				if (typeof offset == "undefined") {
					offset = 0;
				}

				for (let rowNo = 0; rowNo < columnCells.length; rowNo++) {
					let cell = columnCells[rowNo];
					if (offset === 0) {
						cell.isStartOfGroup = true;
					}
					if (offset === cic - 1) {
						cell.isEndOfGroup = true;
					}
					cell.span = maxColumnsInColumn / cic;
					column[rowNo * cic + offset] = cell;
				}
				columnsInsertedInColumn[columnName] = offset + 1;
				foldedCells[targetIndex] = column;
			}

			cells = foldedCells;
			columnHeaders = foldedColumnHeaders;
			columnCount = foldedColumnHeaders.length;
			rowCount *= maxColumnsInColumn;
		}

		if (cells !== undefined && cells.length > 0 && optCasesAcrossColumns) {
			let swapped = swapRowsAndColumns(columnHeaders, cells, optOvertitle);
			cells = swapped.columns;
			columnHeaders = swapped.columnHeaders;
			rowCount = swapped.rowCount;
			columnCount = swapped.columnCount;
		}

		return {'headers': columnHeaders, 'cells': cells}
	},

	_getLaTeXCode: function() {
		/**
		 * Generates the latex code for the table
		 */

		let optSchema = this.model.get('schema');
		let optData = this.model.get('data');
		let optTitle = this.model.get('title');
		let optVariables = this.model.get('variables');
		let optOvertitle = this.model.get('overTitle')
		let optFootnotes = this.model.get('footnotes');

		let data = this._generateLaTeXCode();

		// TODO: footnotes formatting

		let variable = "";  // required to find out the first column
		for (let i = 0; i < optSchema.fields.length; ++i) {
			if (optSchema.fields[i].title === '') {
				variable = optSchema.fields[i].name;
				break;
			}
		}

		let latexCode = "";
		// title
		latexCode += "\\begin{table}[h]\n\t\\centering\n\t\\caption{" + formatCellforLaTeX(optTitle) + "}\n";

		// label
		latexCode += "\t\\label{tab:" + camelize(optTitle) + "}\n\t{\n";

		// alignments - {lrrr}
		let columns = data.headers.length;
		let alignments = '';

		for (let col = 0 ; col < columns; ++col) {
			if (col == 0 || data.headers[col].content === undefined || data.headers[col].content === '') {
				alignments += 'l';
			} else {
				alignments += 'r';
			}
		}

		latexCode += "\t\t\\begin{tabular}{" + alignments + "}\n\t\t\t\\toprule\n\t\t\t";
		// Check if there is overTitle
		let overTitleExists = false;
		for (let i = 0; i < columns; ++i) {
			if (data.headers[i]['overTitle'] !== undefined) {
				overTitleExists = true;
				break;
			}
		}
		let cline_text = '';
		if (overTitleExists) {
			let count = 0;
			let prev = undefined;

			for (let i = 0; i < columns; ++i) {
				let current = data.headers[i]['overTitle'];

				if (i === columns - 1) {
					if (current !== undefined) {
						if (prev === current) {
							count++;
							latexCode += ('\\multicolumn{' + count + '}{c}{'+ formatCellforLaTeX(prev) + '} ');
							cline_text += '\\cline{' + (i - count + 2) + '-' + (i + 1) + '}';
						  } else {
							latexCode += ('\\multicolumn{' + count + '}{c}{'+ formatCellforLaTeX(prev) + '} & ');
							cline_text += '\\cline{' + (i - count + 2) + '-' + (i + 1) + '}';
							latexCode += ('\\multicolumn{1}{c}{'+ current + '} ');
						}
						continue;
					}
				}

				if (current === undefined) {
					if (prev !== undefined) {
						latexCode += ('\\multicolumn{' + count + '}{c}{'+ formatCellforLaTeX(prev) + '} & ');
						cline_text += '\\cline{' + (i - count + 1) + '-' + i + '}';
					}

					count = 0;
					latexCode += ('\\multicolumn{1}{c}{} ');

					if (i < columns - 1) {
						latexCode += '& ';
					}
				} else {
					if (current !== prev && prev !== undefined) {
						latexCode += ('\\multicolumn{' + count + '}{c}{'+ formatCellforLaTeX(prev) + '} & ');
						cline_text += '\\cline{' + (i - count + 1) + '-' + i + '}';
						count = 1;
					} else {
						count++;
					}
				}

				prev = current;
			}

			latexCode += '\\\\\n\t\t\t';
			latexCode += cline_text + '\n\t\t\t';
		}

		for (let i = 0; i < columns; ++i) {
			latexCode += (formatCellforLaTeX(data.headers[i].content) + ' ');
			if (i !== columns - 1) {
				latexCode += '& ';
			}
		}
		latexCode += (' \\\\\n\t\t\t\\cmidrule[0.4pt]{1-' + columns + '}\n');

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
		let previousContent = '-x-';
		for (let r = 0; r < maxRows; ++r) {

			latexCode += '\t\t\t';
			if (maxRows === data.cells[0].length) {
			content = formatCellforLaTeX(data.cells[0][r].content);
				if (previousContent !== content) {
					latexCode += content;
					previousContent = content;
				}
			} else if (incrementFirstCol) {
				latexCode += (formatCellforLaTeX(data.cells[0][firstColRow].content));
			}
			latexCode += (' & ');

			let isStartOfGroup = data.cells[0][firstColRow]['isStartOfGroup'];
			// incrementFirstCol = !isStartOfGroup;

			if (isStartOfGroup === undefined || isStartOfGroup === false) {
				incrementFirstCol = true;
			} else {
				incrementFirstCol = false;
			}

		  for (let c = 1; c < columns; ++c) {
			latexCode += (formatCellforLaTeX(data.cells[c][r].content) + ' ');
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

		latexCode += '\t\t\t\\bottomrule\n';

		if (optFootnotes !== "" && optFootnotes !== null && optFootnotes.length !== 0) {
			latexCode += '\t\t\t% \\addlinespace[1ex]\n';

			for (let i = 0; i < optFootnotes.length; i++) {

				let sym = optFootnotes[i]['symbol'];
				if (Number.isInteger(sym)) {
					sym = '$^{' + sym + '}$';  // FIXME: This is a workaround for now. Use symbol(sym)
				}
				let footnoteText = (formatCellforLaTeX(sym) + ' ');
					footnoteText += formatCellforLaTeX(optFootnotes[i]['text'].replace(/(\r\n|\n|\r|\t)/gm, ''));  // remove line breaks, \n, \r, \r\n, \t

				latexCode += ('\t\t\t% \\multicolumn{' + columns + '}{p{0.5\\linewidth}}{' + footnoteText + '} \\\\\n');
			}
		}

		latexCode += "\t\t\\end{tabular}\n\t}\n\\end{table}\n"

		// Add a comment
		latexCode = "%----- Requires booktabs package -----%\n\\usepackage{booktabs}\n\n" + latexCode;

		return latexCode;
	},

	latexCodeMenuClicked: function () {
		let exportParams = new JASPWidgets.Exporter.params();
		exportParams.format = JASPWidgets.ExportProperties.format.html;
		exportParams.process = JASPWidgets.ExportProperties.process.copy;
		exportParams.htmlImageFormat = JASPWidgets.ExportProperties.htmlImageFormat.temporary;
		exportParams.includeNotes = false;

		let latexCode = this._getLaTeXCode();

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
		'click': '_mouseClicked',
	},

	notePositionBottom: true,

	_hoveringStart: function (e) {
		this.toolbar.setVisibility(true);
	},

	_hoveringEnd: function (e) {
		this.toolbar.setVisibility(false);
	},

	_mouseClicked: function (e) {	},

	hasCollapse: function () {
		return false;
	},

	constructChildren: function (constructor, data) {

		var self = this;
		this.toolbar.selectionElement = function () {
			return self.$el.find('th, td:not(.squash-left)');
		};

		var tablePrimitive = new JASPWidgets.tablePrimitive({ model: this.model, className: "jasp-table-primitive jasp-display-primitive" });
		this.localViews.push(tablePrimitive);
		this.views.push(tablePrimitive);
	},

	titleFormatOverride: 'span',

	disableTitleExport: true,
});

JASPWidgets.tablePrimitive = JASPWidgets.View.extend({

	render: function () {
		var optSchema				= this.model.get("schema");
		var optData					= this.model.get("data");
		var optTitle				= this.model.get("title");
		var optSubtitle				= this.model.get("subtitle");
		var optCasesAcrossColumns	= this.model.get("casesAcrossColumns");
		var optOvertitle			= this.model.get("overTitle")
		var optFootnotes			= this.model.get("footnotes");
		var optCitation				= this.model.get("citation");
		var optStatus				= this.model.get("status");
		var optError				= this.model.get("error");

		var columnDefs		= optSchema.fields
		var columnCount		= columnDefs.length

		let rowData			= optData;
		let rowCount		= rowData.length > 1 ? rowData.length : 1;

		let columnsDict		= createColumns(columnDefs, rowData, optFootnotes);
		let columnHeaders	= columnsDict['columnHeaders'];
		let columns			= columnsDict['columns'];

		var cells			= Array(columnCount);

		for (var colNo = 0; colNo < columnCount; colNo++) {

			var column			= columns[colNo]
			var name			= columnDefs[colNo].name
			var type			= columnDefs[colNo].type
			var format			= columnDefs[colNo].format
			var alignNumbers	= !optCasesAcrossColumns  // numbers can't be aligned across rows
			var combine			= columnDefs[colNo].combine

			cells[colNo]		= formatColumn(column, type, format, alignNumbers, combine, optFootnotes, true)
		}

		var columnsInColumn			= {}  // dictionary of counts
		var columnsInsertedInColumn = {}
		var maxColumnsInColumn		= 0
		var columnNames				= []

		for (var colNo = 0; colNo < columnCount; colNo++)
		{
			var columnName = optSchema.fields[colNo].name
			var subRowPos  = columnName.indexOf("[")

			if (subRowPos != -1)
				columnName = columnName.substr(0, subRowPos)

			columnNames[colNo] = columnName

			var cic = columnsInColumn[columnName]

			cic = (typeof cic == "undefined") ? 1 : cic + 1;

			if (maxColumnsInColumn < cic)
				maxColumnsInColumn = cic

			columnsInColumn[columnName] = cic
		}

		if (maxColumnsInColumn > 1) {  // do columns need to be folded

			var foldedColumnNames	= _.uniq(columnNames)
			var foldedCells			= Array(foldedColumnNames.length)
			var foldedColumnHeaders	= Array(foldedColumnNames.length)

			// fold the headers
			for (var colNo = 0; colNo < foldedColumnNames.length; colNo++)
			{
				var headerIndex				= columnNames.indexOf(foldedColumnNames[colNo])
				foldedColumnHeaders[colNo]	= columnHeaders[headerIndex]
			}


			// fold the columns

			for (var colNo = 0; colNo < columnNames.length; colNo++) {

				var columnCells	= cells[colNo]
				var columnName	= columnNames[colNo]
				var targetIndex = foldedColumnNames.indexOf(columnName)
				var column		= foldedCells[targetIndex]
				var cic			= columnsInColumn[columnName]

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

			cells			 = foldedCells

			columnHeaders	 = foldedColumnHeaders
			columnCount		 = foldedColumnHeaders.length
			rowCount		*= maxColumnsInColumn
		}

		if (cells !== undefined && cells.length > 0 && optCasesAcrossColumns) {

			var swapped		= swapRowsAndColumns(columnHeaders, cells, optOvertitle)
			cells			= swapped.columns
			columnHeaders	= swapped.columnHeaders;
			rowCount		= swapped.rowCount
			columnCount		= swapped.columnCount
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

			var hasOvertitles = false;
			var hasAdjacentOvertitles = false;

			// If we have multiple adjacent overtitles, we should make small
			// breaks in the line under the overTitle to indicate end of old and
			// start of new overTitle. NB: with this option, the line is not copied
			// to text processor.
			var lastOvertitle = "";
			for (var i = 0; i < columnHeaders.length; i++) {
				if (typeof columnHeaders[i].overTitle != "undefined") {
					hasOvertitles = true;
					var overtitle = columnHeaders[i].overTitle;
					if (lastOvertitle != "" && lastOvertitle != overtitle) {
						hasAdjacentOvertitles = true;
						break;
					}
					lastOvertitle = overtitle;
				} else {
					lastOvertitle = "";
				}
			}

			if (hasOvertitles) {

				if (hasAdjacentOvertitles) {
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
						if (hasAdjacentOvertitles) {
							chunks.push('<th colspan="' + (2 * span) + '"><div class="over-title-space">' + oldTitle + '</div></th>');
						} else {
							chunks.push('<th colspan="' + (2 * span) + '">' + oldTitle + '</th>');
						}
						oldTitle = newTitle
						span = 1
					}
				}

				if (newTitle == oldTitle) {
					if (hasAdjacentOvertitles) {
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

			for (var i = 0; i < optFootnotes.length; i++)
				if(optFootnotes[i].text !== "")
				{

					chunks.push('<tr><td colspan="' + 2 * columnCount + '">')

					var footnote = optFootnotes[i]

					if (_.isString(footnote)) {
					  chunks.push(symbol(i) + '&nbsp;')
					  chunks.push(footnote)
					}

					if (_.has(footnote, "symbol")) {
					  if (_.isNumber(footnote.symbol))
						chunks.push(symbol(footnote.symbol) + '&nbsp;')
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
