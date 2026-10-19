var currentLocale	= new Intl.Locale("en");
var currentLocaleId = "en"
var useThousandsSeparators = true

function setCurrentLocaleID(id, useThousandsSeps)
{
	if(currentLocaleId != id)
	{
		currentLocale			= new Intl.Locale(id)
		currentLocaleId			= id
	}
	
	//Lets use that only for money:
	useThousandsSeparators	= useThousandsSeps
}

/*
function formatFixed(value, fixIt) 
{
	return currentLocale.to
}*/


function formatMoney(_currency='EUR', amount, thousands=true) {
	const formatter = new Intl.NumberFormat(currentLocaleId, {
		style: 'currency',
		currency: _currency,
		trailingZeroDisplay: 'stripIfInteger',
		useGrouping: useThousandsSeparators || thousands
	});
	
	return amount == "." ? amount : formatter.format(amount)
}

function handleNoZeroLead(number, result, noZeroLead)
{
	if(number >= 0 && number < 1 && noZeroLead && !result.startsWith("1")) //we need to check whether it starts with 1 despite number < 1 because of rounding in the formatting
		return result.substring(1);
	return result;
}

function formatFixed(number, digitsFrac, noZeroLead=false, thousands=false) {
	if(isNaN(digitsFrac))
		digitsFrac = 0
	const formatter = new Intl.NumberFormat(currentLocaleId, { minimumFractionDigits: digitsFrac, maximumFractionDigits: digitsFrac, useGrouping: thousands});
	
	var result = formatter.format(number)
	
	return handleNoZeroLead(number, result, noZeroLead);
}

function formatPrecision(number, precision, noZeroLead=false, thousands=false) {
	const formatter = new Intl.NumberFormat(currentLocaleId, { minimumSignificantDigits: precision, maximumSignificantDigits: precision, useGrouping: thousands });
	
	var result = formatter.format(number)
	
	return handleNoZeroLead(number, result, noZeroLead);
}

function formatPrecisionWithRespectForFixedDecimals(number, sf, dp, noZeroLead=false, thousands=false) {
	if(isNaN(dp))
		return formatPrecision(number, sf, noZeroLead);
	
	const formatter = new Intl.NumberFormat(currentLocaleId, { 
		minimumSignificantDigits:	sf, maximumSignificantDigits:	sf, 
		minimumFractionDigits:		dp, maximumFractionDigits:		dp, 
		roundingPriority:			"lessPrecision", 
		useGrouping:				thousands 
	});
	
	var result = formatter.format(number)
	
	return handleNoZeroLead(number, result, noZeroLead);
}

function formatNumber(number, thousands=false) {
	const formatter = new Intl.NumberFormat(currentLocaleId, { useGrouping: thousands });
	
	return formatter.format(number)
}

function formatColumn(column, type, format, alignNumbers, combine, modelFootnotes, html = true, errorOnMixed = false) {
	/**
	 * Prepares the columns of a table to the required format
	 * @param column
	 * @param type           column type - string, number, pvalue, integer, ...
	 * @param format         decimal format, p-value format
	 * @param alignNumbers
	 * @param combine
	 * @param modelFootnotes
	 * @param html           html render or latex code. Default is true
	 * @param errorOnMixed   internal, used to avoid infinite loops
	 */

	let columnCells = Array(column.length);

	if (type === "mixed") {

		if (errorOnMixed) {
			// or throw an error? But how?
			return "Error: nested mixed columns are not supported!";
		}

		// call formatColumn for each row of the mixed column
		for (let rowNo = 0; rowNo < column.length; rowNo++) {
			// we need to construct the array [column[rowNo].content.value] because column.length otherwise does not exist.
			columnCells[rowNo] = formatColumn([{content: column[rowNo].content.value}], column[rowNo].content.type, column[rowNo].content.format, alignNumbers, combine, modelFootnotes, html, true)[0];
		}

		return columnCells;

	}

	if (type === "string" || typeof format == "undefined" || format === null) {

		for (let rowNo = 0; rowNo < column.length; rowNo++) {

			let clazz		= (type === "string" && !errorOnMixed) ? "text" : "number";
			let cell		= column[rowNo];
			let content		= cell.content;
			let contentNum	= parseFloat(content)
			let isNumber	= !isNaN(contentNum) && clazz == "number"
			let formatted	= { content: (isNumber ? formatNumber(contentNum) : content) }
			let combined	= false;
			
			formatted["class"] = isNumber ? "number" : clazz

			if (typeof content == "undefined") {
				formatted["content"] = "." 

			} else if (combine && rowNo > 0 && column[rowNo - 1].content == content) {
				formatted["class"]	+= " combined";
				formatted["content"] = html ? "&nbsp;" : " "
				combined = true;
			} else if(!isNumber) {
				if (typeof content === "string" && html)
					content = content.replace(/\u273B/g, "<small>\u273B</small>");
						
				formatted["content"] = content
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

	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////			First collect the formats and their respective settings        ////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	let formats		= format.split(";");
	let p			= NaN;
	let isP			= false;
	let dp			= parseInt(window.globSet.decimals);
	let sf			= NaN;
	let pc			= false;
	let approx		= false;
	let fixDecimals = typeof dp === 'number' && dp >= 0;
	let currency	= ""
	let moneyFmt	= "monetary"
	let noZeroLead	= false;
	let thousands	= false
	
	for (let i = 0; i < formats.length; i++) 
	{
		let f = formats[i];
		if (f.match(/^p:/) !== null) {
			// override APA style if exact p-values wanted
			if (window.globSet.pExact) {
				sf = 4;
			} else {
				p = fixDecimals ? 1/Math.pow(10,dp) : Number(f.substring(2));
				noZeroLead = true;
			}
			
			isP = true;
		}

		if(f.startsWith("thousands"))
			thousands = true
		
		if(f.startsWith(moneyFmt))
		{
			if(f.length > moneyFmt.length) 
			{
				currency = f.substr(moneyFmt.length)
				if(currency.startsWith(":"))
					currency = currency.substring(1)
			}
			
			if(currency == "")
				currency = "EUR"
		}

		if (f.indexOf("dp:") != -1 && !fixDecimals)
			dp = Number(f.substring(3));

		if (f.indexOf("sf:") != -1)
			sf = Number(f.substring(3));

		if (f.indexOf("pc") != -1 || f.indexOf("percentage") != -1)
		{
			pc = true;

			if(!fixDecimals)
				dp = 2	

			let colonPos 	= f.indexOf(":") 
			
			if(colonPos != -1)
			{
				let pcDP = f.substr(colonPos + 1);

				if(!isNaN(parseFloat(pcDP)))
					dp = parseFloat(pcDP)
			}
		}

		if (f.indexOf("~") != -1)
			approx = true;
	}

	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////	  Then try to convert all the cells to float if they werent numbers    ////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	for (var rowNo = 0; rowNo < column.length; rowNo++) {
		let cell = column[rowNo]
		let content = cell.content

		if (typeof(content) !== "number" || typeof(content) !== "Number") {
			if (isNaN(parseFloat(content)))  // isn't a number
				continue
			cell.content = content = parseFloat(content)
		}
	}

	// some vars we need for sf:
	var upperLimit	= 1e6
	var minLSD		= Infinity	// right most position of the least significant digit
	var maxFSDOE	= -Infinity  // left most position of the least significant digit of the exponent in scientific notation
	
	if (isFinite(sf)) {
		//prepare some stuff we need to run "sf"
	
		for (var rowNo = 0; rowNo < column.length; rowNo++) 
		{
			let cell 	= column[rowNo]
			let content = cell.content
	
			if (isNaN(parseFloat(content)))  // isn't a number
				continue
	
			let fsd =fSD(content) // position of first significant digit
			let lsd = fsd - sf
			let fsdoe
	
			if (Math.abs(content) >= upperLimit || Math.abs(content) <= Math.pow(10, -dp))
				fsdoe = fSDOE(content)   // first significant digit of exponent
			
			if (fsdoe > maxFSDOE)
				maxFSDOE = fsdoe
	
			if (lsd < minLSD)
				minLSD = lsd
		}
	
		var _minDp = isFinite(dp) ? -dp : -Infinity;
		minLSD = fixDecimals ? -dp : Math.min(0, Math.max(_minDp, minLSD))
		minLSD = Math.max(-20, minLSD)
	}
	
	format = currency != "" ? "monetary" : pc ? "percentage" : isFinite(sf) ? "significance" : isFinite(dp) ? "decimalPoints" : "other"
	
	//Now that thats been determined we can format our cells
	for (var rowNo = 0; rowNo < column.length; rowNo++) 
	{
		//Get the cell and set up content var everything will read
		//and the formatted var for writing to the new cells at the end.
		let cell 		= column[rowNo]
		let content 	= cell.content
		let formatted	= { content: content, class: ""} 
		let isNumber	= format == "significance" || format == "decimalPoints" || format == "percentage" //Will set class number
		let isPercent	= format == "percentage"		// will set class percentage
	
		if (typeof content == "undefined") 
			formatted["content"] = "."
		else if (content === "" || (combine && rowNo > 0 && column[rowNo - 1].content == content)) 
			formatted["content"] = html ? "&nbsp;" : " "
		else
			switch(format)
			{
				case "other": //Lets try to format it as a number I guess
				{
					let aNumberPerhaps = parseFloat(content)
					
					if(!isNaN(aNumberPerhaps))
					{
						formatted["content"] = formatNumber(aNumberPerhaps, thousands)
						isNumber = true 
					}
					
					break; 
				}
				case "monetary":
				{
					formatted["content"] = formatMoney(_currency=currency, content, thousands)
					formatted["class"]	 = "monetary"
					break;
				}
	
				case "percentage":
				{
					if (!isNaN(parseFloat(content)))
						formatted["content"] = "" + (formatFixed(content * 100, dp, false, thousands)) + (html ? "&thinsp;%" : "%")
					break;
				}
	
				case "significance":
				{
					if (isNaN(parseFloat(content)))  // isn't a number but we'll still give it that class
					{
						formatted["class"] = "number"
						isNumber = false
					}
					else if (content < p) 
					{
						formatted["content"] 	= (html ? "<&nbsp;" : "< ") + formatPrecisionWithRespectForFixedDecimals(p, sf, noZeroLead, thousands)
						formatted["class"]		= "p-value"
						isNumber = false
					}
					else if (content == 0)
					{
						formatted["content"] = (fixDecimals || isFinite(dp)) ? formatFixed(content, dp, noZeroLead, thousands) : formatPrecision(content, sf, false, thousands)
					}
					else if (Math.abs(content) >= upperLimit || Math.abs(content) < Math.pow(10, -dp) || (!fixDecimals && Math.abs(content) >= Math.pow(10, sf)))
					{
						let decimalsExpon 		= fixDecimals ? dp : sf - 1;
						// var paddingNeeded = Math.max(maxFSDOE - fSDOE(content), 0)
						var paddingNeeded = 0
						formatted["content"] 	= toExponential(content, decimalsExpon, paddingNeeded, html)
					}
					else 
					{
						if(isP)						formatted["content"] = fixDecimals || window.globSet.pExact ? toExponential(content, (isFinite(dp) ? dp : sf-1), 0, html)	: formatPrecisionWithRespectForFixedDecimals(content, sf, dp, noZeroLead, thousands)
						else						formatted["content"] = fixDecimals ? formatFixed(content, -minLSD, false, thousands) : formatPrecision(content, sf, false, thousands)
						
						if(html)
							formatted["content"] = formatted["content"].replace(/-/g, "&minus;")
					}
	
					break;
				}
				
				case "decimalPoints":
				{
					if (isNaN(parseFloat(content)))  // isn't a number but we'll give it that class anyway
					{
						formatted["class"] = "number"
						isNumber = false
					}
					else if (content < p) 
					{
						formatted["content"] 	= (html ? "<&nbsp;" : "< ") + formatFixed(p, dp, noZeroLead, thousands)
						formatted["class"]		= "p-value"
						isNumber = false
					}
					else 
					{
						var strContent = "";
		
						if (p && content != 0 && Math.abs(content) < 1/(Math.pow(10,dp))) 
							strContent = toExponential(content, dp, 0, html)
						else 
						{
							strContent = formatFixed(content, dp, noZeroLead, thousands)
							if(html)
								strContent = strContent.replace(/-/g, "&minus;")
						}
						formatted["content"] = strContent
					}
					break;
				}
				
			}
	
		if(isNumber)
			formatted["class"] = "number"
	
		if(isPercent)
			formatted["class"] = "percentage"
	
		if (isNumber && approx) 
			formatted.content = (html ? "~&thinsp;" : "~") + formatted.content
	
		//Finishing up:
		if (typeof cell.footnotes != "undefined")
			formatted["footnotes"] = getFootnotes(modelFootnotes, cell.footnotes)
	
		if (cell.isStartOfGroup)
			formatted["class"] += " new-group-row"
	
		if (cell.isStartOfSubGroup)
			formatted["class"] += " new-sub-group-row"
	
		if (cell.isEndOfGroup)
			formatted["class"] += " last-group-row"
	
		//And at last assign the formatted string to its cell:
		columnCells[rowNo] = formatted
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
	 * Format text to compatible latex code, sort of! This function doens't have any 
	 * idea about the type of the input that is getting, and at the moment it doesn't
	 * account for it. It tries to correct for <sup> and <sub> but it cannot know
	 * whether it's doing it on a data or a text.
	 * 
	 * @param toFormat - string
	 */

	// Rules to convert into latex
	if (toFormat === '&nbsp;') {
		return '';
	}

	let text = toFormat.toString();

	// If text is a number, we put the entire thing inside a math block
	if (!isNaN(text)) {
		return "$" + text + "$";
	}

	let special_match = [  '_',   '%',/*   '$',*/   '&tau;', '&sup2;',   '&', '\u00D7', '\u208A', '\u208B',  '\u223C',  '\u03C7', '\u03A7',  '\u03B7',    '\u03C9', '\u2080', '\u2081', '\u2082', '\u00B2',    '\u03B1',     '\u03BB', '\u273B', '\u2009', '\u2014', '\u273B',    '\u221E']
	let special_repla = ['\\_', '\\%',/* '\\$',*/ '$\\tau$', '$^{2}$', '\\&', '\\times', '$_{+}$', '$_{-}$', '$\\sim$', '$\\chi$',      'X', '$\\eta$', '$\\omega$', '$_{0}$', '$_{1}$', '$_{2}$', '$^{2}$', '$\\alpha$', '$\\lambda$',      '*',      ' ',     '--',      '*', '$\\infty$']


	// Handle special characters
	for (let i = 0; i < special_match.length; ++i) {
		text = (text.split(special_match[i]).join(special_repla[i])).toString();
	}

	// Matching superscripts and subscripts, TODO: handle multiple occurences
	let matched = text.match('<sup>(.*)</sup>');
	if (matched !== null) {

		// If the base is a number as well, then we put the entire text inside
		// the math block as well.
		let splitted = text.split('\\times')
		if (!isNaN(splitted[0])) {
			let formatted = '^{'+ matched[1] + '}';
			text = text.replace(matched[0], formatted);
			text = "$" + text + "$";
		} else {
			let formatted = '$^{'+ matched[1] + '}$';
			text = text.replace(matched[0], formatted);
		}
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
		return index === 0 ? match.toLowerCase() : match.toUpperCase();
	});
}



// It is kind of sad the below is the way to go:
function getDecimalSeparator() 
{
    const numberWithDecimalSeparator = 1.1;
	
    return (new Intl.NumberFormat(currentLocaleId))
		.formatToParts(numberWithDecimalSeparator)
        .find(part => part.type === 'decimal')
        .value;
}

function toExponential(number, decimalsExpon, paddingNeeded, html) {
	let _sign			= (html) ? "&minus;" : "-";
	let exponentiated	= number.toExponential(decimalsExpon).replace(/-/g, _sign).replace(".", getDecimalSeparator())
	let split			= exponentiated.split("e")
	let mantissa		= split[0]
	let exponent		= split[1]
	let padding			= !paddingNeeded 	? ''	: '<span class="do-not-copy" style="visibility: hidden;">' + Array(paddingNeeded + 1).join("0") + '</span>'
	
	let reassembled  = mantissa;
		reassembled += !window.globSet.normalizedNotation ? "e" : (html ? "&times;10" : "×10") + "<sup>"
		reassembled += html ? padding : ""
		reassembled += exponent
		reassembled += !window.globSet.normalizedNotation ? "" : "</sup>"


	return reassembled;
}
