function formatColumn(column, type, format, alignNumbers, combine, modelFootnotes, html = true) {
	/**
	 * Prepares the columns of a table to the required format
	 * @param column
	 * @param type           column type - string, number, pvalue, integer, ...
	 * @param format         decimal format, p-value format
	 * @param alignNumbers
	 * @param combine
	 * @param modelFootnotes
	 * @param html           html render or latex code. Default is true
	 */

	let columnCells = Array(column.length);

	if (type === "string" || typeof format == "undefined") {

		for (let rowNo = 0; rowNo < column.length; rowNo++) {

			let clazz = (type == "string") ? "text" : "number";
			let cell = column[rowNo];
			let content = cell.content;
			let formatted;
			let combined = false;

			if (typeof content == "undefined") {
				formatted = { content: "." }

			} else if (combine && rowNo > 0 && column[rowNo - 1].content == content) {
				clazz += " combined";
				let content = "&nbsp;";
				if (!html) {
					content = " ";
				}
				formatted = { content: content, class: clazz };
				combined = true;

			} else {
				if (typeof content === "string") {
					if (html) {
						content = content.replace(/\u273B/g, "<small>\u273B</small>");
						//content = content.replace(/<(\w)/gi, function(p1, p2) { return '< '+p2; }) //Makes sure there is a space between <char: a<b -> a< b // breaks any tag used in cells... like the above <small>
					}
				}
				formatted = { content: content, "class": clazz };
			}

			if (combined == false && cell.isStartOfGroup)
				formatted.isStartOfGroup = true;

			if (cell.isStartOfSubGroup)
				formatted.isStartOfSubGroup = true;

			if (cell.isEndOfGroup)
				formatted.isEndOfGroup = true;

			if (typeof cell.footnotes != "undefined")
				formatted.footnotes = getFootnotes(modelFootnotes, cell.footnotes);

			columnCells[rowNo] = formatted;
		}
		return columnCells
	}

	let formats = format.split(";");
	let p = NaN;
	let dp = NaN;
	let sf = NaN;
	let pc = false;
	let approx = false;
	let log10 = false;
	dp = parseInt(window.globSet.decimals);  // NaN if not specified by user
	let fixDecimals = (typeof dp === 'number') && (dp % 1 === 0);

	for (let i = 0; i < formats.length; i++) {

		let f = formats[i];
		if (f.match(/^p:/) !== null) {
			// override APA style if exact p-values wanted
			if (window.globSet.pExact) {
				sf = 4;
			} else {
				p = f.substring(2);
			}
		}

		if (f.indexOf("dp:") != -1 && !fixDecimals)
			dp = f.substring(3);

		if (f.indexOf("sf:") != -1)
			sf = f.substring(3);

		if (f.indexOf("pc") != -1)
			pc = true;

		if (f.indexOf("~") != -1)
			approx = true;

		if (f.indexOf("log10") != -1)
			log10 = true;
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
				fsd = fSD(content)

			var lsd = fsd - sf

			if (log10) {

				if (content >= 6 || content <= -dp) {

					fsdoe = fSD(content)

					if (fsdoe > maxFSDOE)
						maxFSDOE = fsdoe
				}

			} else if (Math.abs(content) >= upperLimit || Math.abs(content) <= Math.pow(10, -dp)) {

				var fsdoe   // first significant digit of exponent

				fsdoe = fSDOE(content)

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

					var paddingNeeded = Math.max(maxFSDOE - fSD(content), 0)

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
				var paddingNeeded = Math.max(maxFSDOE - fSDOE(content), 0)

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
				formatted.footnotes = getFootnotes(modelFootnotes, cell.footnotes)

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
				var strContent;

				if (content < 1/(Math.pow(10,dp))) {
					let _sign = (html) ? "&minus;" : "-";
					var exponentiated = content.toExponential(dp).replace(/-/g, _sign)

					var split = exponentiated.split("e")
					var mantissa = split[0]
					var exponent = split[1]
					var exponentSign = exponent.substr(0, 1)
					var exponentNum = exponent.substr(1)

					if (html) {
						strContent = mantissa + "e&thinsp;" + exponentSign + exponentNum;
					} else {
						strContent = mantissa + "e" + exponentSign + exponentNum;
					}

				} else {
					let _content = (html) ? "&minus;" : "-";
					strContent = content.toFixed(dp).replace(/-/g, _content)
				}
				formatted = { content: strContent, "class": "number" }
				isNumber = true
			}

			if (typeof cell.footnotes != "undefined")
				formatted.footnotes = getFootnotes(modelFootnotes, cell.footnotes)

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
				formatted.footnotes = getFootnotes(modelFootnotes, cell.footnotes)

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
				formatted.footnotes = getFootnotes(modelFootnotes, cell.footnotes)

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

}

function createColumns(columnDefs, rowData, modelFootnotes) {
	/**
	 * Returns 'columns' data array
	 * @param columnDefs     Schema (fields)
	 * @param rowData        Table data
	 * @param modelFootnotes
	 */

	let columnCount		= columnDefs.length;
	let columns			= Array(columnCount);
	let columnHeaders	= Array(columnCount);
	let tableDataExists = rowData.length > 0;
	let rowCount		= tableDataExists ? rowData.length : 1;

	for (let colNo = 0; colNo < columnCount; colNo++) {

		// populate column headers
		let columnDef	= columnDefs[colNo];
		let columnName	= columnDef.name;
		let title		= columnDef.title;
		let overTitle	= columnDef.overTitle;

		if (typeof title == "undefined")	title = columnName;
		if (title == "")					title = "&nbsp;";

		let columnType		= columnDef.type;
		let columnHeader	= {content: title, header: true, type: columnType};

		if (overTitle)     columnHeader.overTitle = overTitle;

		if (typeof columnDef[".footnotes"] != "undefined")
			columnHeader.footnotes = getFootnotes(modelFootnotes, columnDef[".footnotes"]);

		columnHeaders[colNo] = columnHeader;

		// populate cells column-wise
		let column		= Array(rowCount);
		let isGrouped	= false;

		for (let rowNo = 0; rowNo < rowCount; rowNo++) {

			let row = [];
			let content = '.';

			if (tableDataExists) {
				row = rowData[rowNo];
				content = row[columnName] == null ? '' : row[columnName];
			}          

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
		'columns':			columns,
		'columnHeaders':	columnHeaders
	};
}

function fSD(value) {
	/**
	 * First significant digit position
	 * @param value
	 */

	if (value > 0)
		return Math.floor(Math.log(+value) / Math.log(10)) + 1
	else if (value < 0)
		return Math.floor(Math.log(-value) / Math.log(10)) + 1
	else
		return 1

}

function fSDOE(value) {
	/**
	 * First significant digit position in the exponent in scientific notation
	 * @param value
	 */

	if (value == 0)
		return 1

	var exponent = Math.floor(Math.log(Math.abs(value)) / Math.log(10))

	return fSD(exponent)

}

function getFootnotes(optFootnotes, indices) {
	/**
	 * Get footnotes for table
	 * @param optFootnotes footnotes for the current table
	 * @param indices
	 */

	let footnotes = Array(indices.length);

	for (let i = 0; i < indices.length; i++) {
		let index = indices[i];

		if (_.isString(index)) {
			footnotes[i] = index;
		} else if (index < optFootnotes.length) {

			let footnote = optFootnotes[index];
			if (typeof footnote.symbol == "undefined")
				footnotes[i] = symbol(index);
			else if (_.isNumber(footnote.symbol))
				footnotes[i] = symbol(footnote.symbol);
			else
				footnotes[i] = footnote.symbol;
		} else {
			footnotes[i] = symbol(index);
		}
	}

	return footnotes
}

function symbol(index) {
	/**
	 * Get \u symbol
	 * @param index
	 */

	return ("\u1D43\u1D47\u1D9C\u1D48\u1D49\u1DA0\u1D4D\u02B0\u2071\u02B2\u1D4F\u02E1\u1D50\u207F\u1D52\u1D56\u02B3\u02E2\u1D57\u1D58\u1D5B\u02B7\u02E3\u02B8\u1DBB").charAt(index)
}

function swapRowsAndColumns (columnHeaders, columns, optOvertitle) {
	/**
	 * Swap columns and rows (descriptive tables - input data is reversed. Also other places perhaps?)
	 * @param columnHeaders
	 * @param columns
	 * @param optOvertitle
	 */

	var newColumnHeaders;
	let newRowCount			= columns.length - 1;
	let newColumnCount		= columns[0].length + 1;
	var originalOvertitles	= Array(columnHeaders.length);
	var overtitleFound		= false;

	for(let colNo = 0; colNo < originalOvertitles.length; colNo++)
	{
		//The first columnheader should be ignored, because the rows get pasted *underneath* the columnheaders and that would make them one step too far
		originalOvertitles[colNo] = columnHeaders[colNo]["overTitle"] !== undefined ? columnHeaders[colNo]["overTitle"] : ""
		if(originalOvertitles[colNo] !== "")
			overtitleFound = true;
	}

	if(!overtitleFound)
		originalOvertitles = null;


	if (optOvertitle)
	{
		// Transform first column into overtitle, second into title
		newColumnHeaders = Array(newColumnCount-1);

		for (let colNo = 0; colNo < newColumnCount - 1; colNo++) {
			newColumnHeaders[colNo] = {
				content:	columns[1][colNo].content,
				header:		true,
				overTitle:	columns[0][colNo].content,
				type:		"string"
			};
		}
		// remove the column that became title
		columns.shift();
		columnHeaders.shift();
		newRowCount--;
	} else
		newColumnHeaders = columns[0];

	let newColumns = Array(newColumnCount);
	let cornerCell = columnHeaders.shift();
	newColumnHeaders.unshift(cornerCell);
	newColumns[0] = headerCellsToDataCells(columnHeaders);

	for (let colNo = 1; colNo < newColumnCount; colNo++) {
		newColumns[colNo] = Array(newRowCount);

		for (let rowNo = 0; rowNo < newRowCount; rowNo++) {
			newColumns[colNo][rowNo] = columns[rowNo + 1][colNo - 1];
		}
	}

	if(originalOvertitles !== null)
	{
		for(var col=0; col<newColumnHeaders.length; col++)
			newColumnHeaders[col]["overTitle"] = undefined

		var newColHeaderOvertitle = originalOvertitles[0] === undefined ? "" : originalOvertitles[0]
		if(newColHeaderOvertitle === "") newColHeaderOvertitle = "&nbsp;";

		newColumnHeaders.unshift(
		{
			content:	newColHeaderOvertitle,
			header:		true,
			type:		"string"
		});

		originalOvertitles.shift(); //drop first element

		newColumns.unshift(Array(newRowCount));
		newColumnCount++;

		var lastOvertitle = "";

		for (let rowNo = 0; rowNo < newRowCount; rowNo++)
		{
			var curOvertitle = "";

			if (rowNo < originalOvertitles.length)
			{
				curOvertitle = originalOvertitles[rowNo];

				if (curOvertitle === lastOvertitle)
					curOvertitle = "";

				lastOvertitle		 = originalOvertitles[rowNo]
			}

			newColumns[0][rowNo] = {
				content:	curOvertitle,
				class:		"text"
			}
		}

		newColumns = addGroupsToTransposedTableWithOverTitles(newColumns);
	}

	return {
		columnHeaders:	newColumnHeaders,
		columns:		newColumns,
		rowCount:		newRowCount,
		columnCount:	newColumnCount
	}
}

function addGroupsToTransposedTableWithOverTitles(columns) {
	var rowNameCol = columns[0];
	if (rowNameCol.length == 0)
		return columns;

	var uniqueRowNames = Array();
	for (var rowNo = 0; rowNo < rowNameCol.length; rowNo++) {
		var rowName = rowNameCol[rowNo].content;
		if (uniqueRowNames.indexOf(rowName) === -1)
			uniqueRowNames.push(rowName);
	}

	// we have groupings if there are more rows than unique row names
	if (uniqueRowNames.length < rowNameCol.length) {
		var startOfGroupIndices = Array();
		var endOfGroupIndices = Array();

		for (var rowNo = 0; rowNo < rowNameCol.length; rowNo++) {
			var curGroupName = rowNameCol[rowNo].content;
			if (curGroupName !== "")
				startOfGroupIndices.push(rowNo);
				
			if (rowNo + 1 < rowNameCol.length) {
				var nextGroupName = rowNameCol[rowNo + 1].content;
				if (curGroupName === "" && nextGroupName !== "")
					endOfGroupIndices.push(rowNo);
			} else { // the last row always closes a group
				endOfGroupIndices.push(rowNo);
			}
		}

		for (var colNo = 0; colNo < columns.length; colNo++) {
			for (var startOfGroupIndex of startOfGroupIndices)
				columns[colNo][startOfGroupIndex].isStartOfGroup = true;
			
			for (var endOfGroupIndex of endOfGroupIndices)
				columns[colNo][endOfGroupIndex].isEndOfGroup = true;
		}
	}
	return(columns)
}

function headerCellsToDataCells(cells) {
	for (var i = 0; i < cells.length; i++) {
		cells[i].class		= "text";
		cells[i].header		= undefined;
		cells[i].type		= undefined;
		cells[i].overTitle	= undefined;
	}
	return cells;
}

function formatCellforLaTeX (toFormat) {
	/**
	 * Format text to compatible latex code
	 * @param toFormat - string
	 */

	// Rules to convert into latex
	if (toFormat === '&nbsp;') {
		return '';
	}
	let text = toFormat.toString();
	let special_match = [  '_',   '%',/*   '$',*/   '&tau;', '&sup2;',   '&', '\u208A', '\u208B',  '\u223C',  '\u03C7', '\u03A7',  '\u03B7',    '\u03C9', '\u2080', '\u2081', '\u2082', '\u00B2',    '\u03B1',     '\u03BB', '\u273B', '\u2009', '\u2014', '\u273B',    '\u221E']
	let special_repla = ['\\_', '\\%',/* '\\$',*/ '$\\tau$', '$^{2}$', '\\&', '$_{+}$', '$_{-}$', '$\\sim$', '$\\chi$',      'X', '$\\eta$', '$\\omega$', '$_{0}$', '$_{1}$', '$_{2}$', '$^{2}$', '$\\alpha$', '$\\lambda$',      '*',      ' ',     '--',      '*', '$\\infty$']

	// Handle special characters
	for (let i = 0; i < special_match.length; ++i) {
		text = (text.split(special_match[i]).join(special_repla[i])).toString();
	}

	// match superscripts and subscripts, TODO: handle multiple occurences
	let matched = text.match('<sup>(.*)</sup>');
	if (matched !== null) {
		let formatted = '$^{'+ matched[1] + '}$';
		text = text.replace(matched[0], formatted);
	}

	matched = text.match('<sub>(.*)</sub>');
	if (matched !== null) {
		let formatted = '$\_{'+ matched[1] + '}$';
		text = text.replace(matched[0], formatted);
	}

	matched = text.match('<em>(.*)</em>');
	if (matched !== null) {
		let formatted = '\\textit{' + matched[1] + '}';
		text = text.replace(matched[0], formatted);
	}

	let special_match_after = ['<'];
	let special_repla_after = ['$<$'];

	for (let i = 0; i < special_match_after.length; ++i) {
		text = (text.replace(special_match_after[i], special_repla_after[i])).toString();
	}

	return text
}

function camelize (str) {
	return str.replace(/(?:^\w|[A-Z]|\b\w|\s+)/g, function(match, index) {
		if (+match === 0) return "";
		return index == 0 ? match.toLowerCase() : match.toUpperCase();
	});
}
