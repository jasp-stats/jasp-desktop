//don't yet support [toc]
//or links
// trim #'s and spaces from end of heading but the closing #'s must have a space before eg. ## fsdfsdf ## not ## dfsdf##
// support html
//headings, hline, ... in html to markdown
//tables

/*Tables aren't part of the core Markdown spec, but they are part of GFM and Showdown supports them by turning on the flag `tables`.

```
| Tables        | Are           | Cool  |
| ------------- |:-------------:| -----:|
| **col 3 is**  | right-aligned | $1600 |
| col 2 is      | *centered*    |   $12 |
| zebra stripes | ~~are neat~~  |    $1 |*/


var Mrkdwn = {
	fromDOMElement: function (element) {
		var contents = element.contents();

		var markdown = null;

		var data = { level: 0, data0: { topLevel: true }, lineBaseSymbols: [] };

		for (var i = 0; i < contents.length; i++) {
			var node = contents[i];

			var lineData = this._processHtmlNode(node, data);

			for (var j = 0; j < lineData.lines.length; j++) {
				if (markdown === null)
					markdown = lineData.lines[j];
				else if (j === 0 && lineData.append)
					markdown = markdown + lineData.lines[j];
				else
					markdown = markdown + "\n" + lineData.lines[j];
			}
		}

		if (markdown === null)
			markdown = '';

		return markdown;
	},

	fromHtmlText: function (html) {
		var $element = $(document.createElement('div'));
		$element.html(html);
		return this.fromDOMElement($element);
	},

	toHtml: function (markdown) {
		var data = { level: 0, data0: {}, ignore: {} };
		return this._sortMarkdownLines(markdown, data);
	},

	_pushToNextLevel: function (node, data, levelData, getFunction) {
		data.level += 1;
		data['data' + data.level] = levelData;
		var parsed = getFunction.call(this, node, data);
		//delete data['data' + data.level];
		data.level -= 1;
		return parsed;
	},

	_getLevelData: function (data) {
		return data['data' + data.level]
	},

	_checkForChar: function (text, index, char) {
		return index > 0 && index < text.length && text[index] === char;
	},

	_isLineBreak: function (text, index) {
		if (index < 2 || index + 2 >= text.length)
			return false;

		return this._isSpace(text, index) && this._isSpace(text, index + 1) && text[index + 2] === '\n';
	},

	_isCharEscaped: function (text, index) {
		if (index === 0)
			return false;

		var slashCount = 0;
		while (text[index - slashCount - 1] === '\\')
			slashCount += 1;

		return (slashCount % 2) === 1;
	},

	_hasPrespace: function (text, index) {
		if (index === 0)
			return false;

		return this._isWhiteSpace(text, index - 1);
	},

	_hasPostspace: function (text, index) {
		if (index === text.length - 1)
			return false;

		return this._isWhiteSpace(text, index + 1);
	},

	_hasPrespaceFiltered: function (text, index) {
		if (index === 0)
			return true;

		if (this._isWhiteSpace(text, index - 1))
			return true;

		var specialChars = ['~', '*', '_', '`'];
		for (var i = 0; i < specialChars.length; i++) {
			if (text[index - 1] === specialChars[i]) {
				if (this._isCharEscaped(text, index - 1))
					return false;
				else
					return this._hasPrespaceFiltered(text, index - 1)
			}
		}

		return false;
	},

	_hasPostspaceFiltered: function (text, index) {
		if (index === text.length - 1)
			return true;

		if (this._isWhiteSpace(text, index + 1))
			return true;

		var specialChars = ['~', '*', '_', '`'];
		for (var i = 0; i < specialChars.length; i++) {
			if (text[index + 1] === specialChars[i]) {
				if (this._isCharEscaped(text, index + 1))
					return false;
				else
					return this._hasPostspaceFiltered(text, index + 1)
			}
		}

		return false;
	},


	_isWhiteSpace: function (text, index) {
		return text[index] === ' ' || text[index] === '\xA0' || text[index] === '\t';
	},

	_isSpace: function (text, index) {
		return text[index] === ' ' || text[index] === '\xA0';
	},

	_howManyWhiteSpace: function (text, index, endIndex) {

		var end = endIndex === -1 ? text.length : endIndex;
		var otherChars = false;
		var tabcount = 0;
		var spaceCount = 0;
		for (var i = index; i < end; i++) {
			if (text[i] === '\t')
				tabcount += 1;
			else if (this._isSpace(text, i))
				spaceCount += 1;
			else {
				otherChars = true;
				break;
			}
		}
		return { tabcount: tabcount, spaceCount: spaceCount, total: tabcount + spaceCount, hasOther: otherChars, indentCount: (tabcount*4) + spaceCount };
	},

	_howManyDigits: function (text, index, endIndex) {
		var count = 0;
		var end = endIndex === -1 ? text.length - 1 : endIndex;
		for (var i = index; i < end; i++) {
			var char = text[i];
			if (char <= '9' && char >= '0')
				count += 1;
			else
				break;
		}
		return count;
	},

	_howMany: function (text, index, endIndex, char) {

		var end = endIndex === -1 ? text.length : endIndex;
		var otherChars = false;
		var count = 0;
		var sc = 0;
		var spaceCount = 0;
		for (var i = index; i < end; i++) {
			var c = text[i];

			if (c === char) {
				count += 1;
				spaceCount = sc;
			}
			else if (this._isSpace(text, i))
				sc += 1;
			else {
				otherChars = true;
				break;
			}
		}
		return { count: count, spaceCount: spaceCount, total: count + spaceCount, hasOther: otherChars };
	},

	_countCommonChars: function (text, index, max) {
		var count = 1;
		for (var i = index; i < text.length - 1; i++) {
			if (max !== undefined && count === max)
				break;

			if (text[i] === text[i + 1])
				count += 1;
			else
				break;
		}
		return count;
	},

	_findNextUnescapedChar: function (text, index, targetChar) {
		var next = text.indexOf(targetChar, index);
		while (this._isCharEscaped(text, next)) {
			next = text.indexOf(targetChar, next + 1);
		}
		return next;
	},

	_findNextAcceptableEndCommand: function (text, index, endIndex, targetChar, needsOuterSpace, needsInnerConnection) {
		var next = text.indexOf(targetChar, index);
		while (next !== -1 && next < endIndex && (this._isCharEscaped(text, next) || (needsOuterSpace && !this._hasPostspaceFiltered(text, next)) || (needsInnerConnection && this._hasPrespaceFiltered(text, next)))) {
			next = text.indexOf(targetChar, next + 1);
		}
		return next;
	},

	_addIgnoreList: function (data, charList) {
		for (var i = 0; i < charList.length; i++) {
			var char = charList[i];
			if (data.ignore[char] === undefined) {
				data.ignore[char] = 1;
			} else {
				data.ignore[char] += 1;
			}
		}
	},

	_removeIgnoreList: function (data, charList) {
		for (var i = 0; i < charList.length; i++)
			data.ignore[charList[i]] -= 1;
	},


	_checkWhatsPossible: function (type, count) {
		var countUsed = count;
		var functionName = '_processMrkdown_' + type + '_';
		var parseFunctionName = functionName + countUsed.toString();

		while (this[parseFunctionName] === undefined && countUsed > 0) {
			countUsed -= 1;
			parseFunctionName = functionName + countUsed.toString();
		}

		return countUsed;
	},

	_sortMarkdownLines: function (text, data) {
		var levelData = this._getLevelData(data);

		levelData.objects = [];
		levelData.indentDepth = [0];
		levelData.relativeIndentDepth = [0];
		var line;
		var previousLine = null;

		var index = text.indexOf("\n");
		var linestart = 0;
		while (index !== -1) {
			if (this._isLineBreak(text, index - 2) === false) {
				line = this._getLineDetails(text, linestart, index, 0, null, previousLine);
				if (line !== undefined) {
					previousLine = line;
					levelData.objects.push(line);
				}
				linestart = index + 1;
			}
			index = text.indexOf("\n", index + 1);
		}

		if (linestart !== text.length - 1) {
			line = this._getLineDetails(text, linestart, text.length - 1, 0, null, previousLine);
			if (line !== undefined)
				levelData.objects.push(line);
		}

		var parsed = '';
		levelData.currentObjIndex = 0;
		while (levelData.currentObjIndex < levelData.objects.length) {
			var obj = levelData.objects[levelData.currentObjIndex];

			parsed += this._processIndentedMrkdownNode(obj, data, false);

			levelData.currentObjIndex += 1;
		}

		return parsed;
	},

	_checkForObject_multiline_code: function (text, index, endIndex, groupLevel, parent, previousLine, whitespace) {
		if (parent !== null) //isRoot === true
			return null;

		if (text[index] !== '`' || text[index + 1] !== '`' || text[index + 2] !== '`')
			return null;

		var charCount = this._howMany(text, index + 3, endIndex, '`').count + 3;

		var obj = new Mrkdwn.markdownLineDetails('multiline_code', charCount, 1, false, groupLevel, 'mc' + charCount.toString());
		obj.isContainer = false;
		return obj;
	},

	_processMrkdownObj_multiline_code: function (obj, data) {
		var levelData = this._getLevelData(data);
		var parsed = ''
		if (obj.getLine().indexOf(obj.getModifier(), obj.modifierLength) !== -1)
			parsed += this._processMrkdownObj_text(obj, data);
		else {
			parsed = '<pre><code>';
			if (levelData.currentObjIndex + 1 < levelData.objects.length) {
				var next = levelData.objects[levelData.currentObjIndex + 1];
				while (next.id !== 'multiline_code') {
					levelData.currentObjIndex += 1;
					levelData.trimSpaces = false;
					parsed += next.getLine().replace(/&/g, '&amp;').replace(/>/g, '&gt;').replace(/</g, '&lt;');
					levelData.trimSpaces = true;

					parsed += '\n';

					if (levelData.currentObjIndex + 1 >= levelData.objects.length)
						break;
					next = levelData.objects[levelData.currentObjIndex + 1];
				}
				levelData.currentObjIndex += 1;
			}
			parsed += '</code></pre>';
		}

		return parsed;
	},

	_trimIndent: function(text, indentCount) {
		var charCount = 0;
		var trimmed = 0;
		var paddingNeeded = 0;
		for (var i = 0; i < text.length; i++) {
			if (this._isSpace(text, i))
				trimmed += 1;
			else if (text[i] === '\t') {
				var portion = 4 - (trimmed % 4);
				if (portion + trimmed > indentCount) {
					paddingNeeded = portion + trimmed - indentCount;
					portion = indentCount - trimmed;
				}
				trimmed += portion;
			}
			else
				break;
			
			charCount += 1;

			if (trimmed === indentCount)
				break;
		}

		if (charCount > 0)
			return text.substring(charCount);

		return text;
	},

	_processMrkdownObj_code: function (obj, data) {
		var levelData = this._getLevelData(data);

		var indentDepth = levelData.indentDepth[levelData.indentDepth.length - 1];

		var parsed = '<pre><code>';
		//if (obj.inner !== undefined)
		parsed += this._trimIndent(obj.getLine(), levelData.rootIndentDepth + 4).replace(/&/g, '&amp;').replace(/>/g, '&gt;').replace(/</g, '&lt;') + '\n';

		if (levelData.currentObjIndex + 1 < levelData.objects.length) {
			var next = levelData.objects[levelData.currentObjIndex + 1].levelup(obj);
			var isCode = (next.leadingWhiteSpace.indentCount - indentDepth) >= 4;
			var blankLine = 0;
			while (isCode || next.isEmpty) {

				levelData.currentObjIndex += 1;
				if (isCode) {
					for (var i = 0; i < blankLine; i++)
						parsed += '\n';
					parsed += this._trimIndent(next.getLine(), indentDepth + 4).replace(/&/g, '&amp;').replace(/>/g, '&gt;').replace(/</g, '&lt;');
					blankLine = 0;
				}

				blankLine += 1;

				if (levelData.currentObjIndex + 1 >= levelData.objects.length)
					break;
				next = levelData.objects[levelData.currentObjIndex + 1].levelup(next);
				isCode = (next.leadingWhiteSpace.indentCount - indentDepth) >= 4;
			}
		}
		parsed += '</code></pre>';
		return parsed;
	},

	_checkForObject_ol: function (text, index, endIndex, groupLevel, parent, previousLine, whitespace) {

		if (previousLine !== null) {
			var previousBase = previousLine;
			if (parent !== null) {
				previousBase = previousLine.levelup(parent);
				if (previousBase.isContainer)
					previousBase = previousBase.inner;
			}

			if (!previousBase.isEmpty && (previousBase.id !== 'ul' && previousBase.id !== 'ol'))
				return null;
		}

		var startIndex = index + whitespace.total;

		var dCount = this._howManyDigits(text, startIndex, endIndex);
		if (dCount === 0)
			return null;

		if (text[startIndex + dCount] !== '.' && text[startIndex + dCount] !== ')')
			return null;

		if (this._isWhiteSpace(text, startIndex + dCount + 1) === false)
			return null;

		var obj = new Mrkdwn.markdownLineDetails('ol', dCount + 2, 1, true, groupLevel);
		obj.isContainer = false;
		return obj;
	},

	_processMrkdownObj_ol: function (obj, data) {
		var levelData = this._getLevelData(data);
		var parsed = '<ol>';
		var next = obj;
		var firstNode = true;
		var terminate = false;

		var topLevelIndent = obj.leadingWhiteSpace.indentCount;
		levelData.indentDepth.push(topLevelIndent);

		var paragraphCheck = levelData.objects[levelData.currentObjIndex + 1].levelup(obj);
		if (levelData.paragraphOverride === undefined)
			levelData.paragraphOverride = [];
		levelData.paragraphOverride.push(paragraphCheck !== null && paragraphCheck.isEmpty && paragraphCheck.groupLevel === obj.base().groupLevel);

		while (next.id === 'ol') {
			if (!firstNode)
				levelData.currentObjIndex += 1;

			parsed += '<li>';
			parsed += this._processIndentedMrkdownNode(next.inner, data, true);

			var previous = next;
			if (levelData.currentObjIndex + 1 < levelData.objects.length) {
				next = levelData.objects[levelData.currentObjIndex + 1].levelup(next);
				if (next.isEmpty) {
					levelData.currentObjIndex += 1;
					if (levelData.currentObjIndex + 1 < levelData.objects.length)
						next = levelData.objects[levelData.currentObjIndex + 1].levelup(next);
					else
						terminate = true;
				}

				if (!terminate) {
					var childIndent = topLevelIndent + 3;
					levelData.indentDepth.push(childIndent);
					var indent = next.leadingWhiteSpace.indentCount;
					while (indent >= childIndent) {
						levelData.currentObjIndex += 1;
						parsed += this._processIndentedMrkdownNode(next, data, false);

						if (levelData.currentObjIndex + 1 >= levelData.objects.length)
							break;

						next = levelData.objects[levelData.currentObjIndex + 1].levelup(next);
						if (next.isEmpty) {
							levelData.currentObjIndex += 1;
							next = levelData.objects[levelData.currentObjIndex + 1].levelup(next);
						}

						indent = next.leadingWhiteSpace.indentCount;
					}
					if (indent < topLevelIndent)
						terminate = true;
					levelData.indentDepth.pop();
				}
			}

			parsed += '</li>';

			if (terminate || levelData.currentObjIndex + 1 >= levelData.objects.length)
				break;

			firstNode = false;
		}
		parsed += '</ol>';
		levelData.indentDepth.pop();

		levelData.paragraphOverride.pop();
		return parsed;
	},

	_checkForObject_ul: function (text, index, endIndex, groupLevel, parent, previousLine, whitespace) {

		if (previousLine !== null) {
			var previousBase = previousLine;
			if (parent !== null) {
				previousBase = previousLine.levelup(parent);
				if (previousBase.isContainer)
					previousBase = previousBase.inner;
			}

			if (!previousBase.isEmpty && (previousBase.id !== 'ul' && previousBase.id !== 'ol'))
				return null;
		}

		var startIndex = index + whitespace.total;
		var char = text[startIndex];
		if (char !== '*' && char !== '+' && char !== '-')
			return null;

		if (this._isWhiteSpace(text, startIndex + 1) === false)
			return null;

		var obj = new Mrkdwn.markdownLineDetails('ul', 2, 1, true, groupLevel);
		obj.isContainer = false;
		return obj;
	},

	_processMrkdownObj_ul: function (obj, data) {
		var levelData = this._getLevelData(data);
		var parsed = '<ul>';
		var next = obj;
		var firstNode = true;
		var terminate = false;

		var topLevelIndent = obj.leadingWhiteSpace.indentCount;
		levelData.indentDepth.push(topLevelIndent);

		var paragraphCheck = levelData.objects[levelData.currentObjIndex + 1].levelup(obj);
		if (levelData.paragraphOverride === undefined)
			levelData.paragraphOverride = [];
		levelData.paragraphOverride.push(paragraphCheck !== null && paragraphCheck.isEmpty && paragraphCheck.groupLevel === obj.base().groupLevel);

		while (next.id === 'ul') {
			if (!firstNode)
				levelData.currentObjIndex += 1;

			parsed += '<li>';
			parsed += this._processIndentedMrkdownNode(next.inner, data, true);

			var previous = next;
			if (levelData.currentObjIndex + 1 < levelData.objects.length) {
				next = levelData.objects[levelData.currentObjIndex + 1].levelup(next);
				if (next.isEmpty) {
					levelData.currentObjIndex += 1;
					if (levelData.currentObjIndex + 1 < levelData.objects.length)
						next = levelData.objects[levelData.currentObjIndex + 1].levelup(next);
					else
						terminate = true;
				}

				if (!terminate) {
					var childIndent = topLevelIndent + 2;
					levelData.indentDepth.push(childIndent);
					var indent = next.leadingWhiteSpace.indentCount;
					while (indent >= childIndent) {
						levelData.currentObjIndex += 1;
						parsed += this._processIndentedMrkdownNode(next, data, false);

						if (levelData.currentObjIndex + 1 >= levelData.objects.length)
							break;

						next = levelData.objects[levelData.currentObjIndex + 1].levelup(next);
						if (next.isEmpty) {
							levelData.currentObjIndex += 1;
							next = levelData.objects[levelData.currentObjIndex + 1].levelup(next);
						}

						indent = next.leadingWhiteSpace.indentCount;
					}
					if (indent < topLevelIndent)
						terminate = true;
					levelData.indentDepth.pop();
				}
			}

			parsed += '</li>';

			if (terminate || levelData.currentObjIndex + 1 >= levelData.objects.length)
				break;

			firstNode = false;
		}
		parsed += '</ul>';
		levelData.indentDepth.pop();

		levelData.paragraphOverride.pop();
		return parsed;
	},

	_processIndentedMrkdownNode: function (obj, data, isInner) {
		var parsed;
		var levelData = this._getLevelData(data);
		var offset = isInner ? 0 : levelData.indentDepth[levelData.indentDepth.length - 1];
		levelData.rootIndentDepth = offset;

		if (obj.leadingWhiteSpace.indentCount - offset >= 4) {
			parsed = this._processMrkdownObj_code(obj, data);
		}
		else {
			parsed = this['_processMrkdownObj_' + obj.id](obj, data);
		}

		return parsed;
	},

	_checkForObject_quote: function (text, index, endIndex, groupLevel, parent, previousLine, whitespace) {
		if (previousLine !== null) {
			var previous = previousLine;
			if (parent !== null)
				previous = previousLine.levelup(parent).inner;
			if (previous === undefined || previous.groupLevel !== groupLevel)
				return null;

			if (!previous.isEmpty && previous.id !== 'quote')
				return null;
		}

		var startIndex = index + whitespace.total;
		var charCount = this._howMany(text, startIndex, endIndex, '>');

		if (charCount.count === 0)
			return null;

		var obj = new Mrkdwn.markdownLineDetails('quote', charCount.total, charCount.count, true, groupLevel, 'q' + charCount.count.toString());
		obj.isContainer = true;
		return obj;
	},

	_processMrkdownObj_quote: function (obj, data) {
		var levelData = this._getLevelData(data);
		var count = obj.modifierCount;
		var parsed = '<blockquote>';
		if (obj.inner !== undefined)
			parsed += this._processIndentedMrkdownNode(obj.inner, data, true);
		//parsed += this['_processMrkdownObj_' + obj.inner.id](obj.inner, data);

		if (levelData.currentObjIndex + 1 < levelData.objects.length) {
			var next = levelData.objects[levelData.currentObjIndex + 1];
			while (next.id === 'quote' && next.modifierCount >= count) {
				levelData.currentObjIndex += 1;

				if (next.id === 'quote' && next.modifierCount > count)
					parsed += this._processMrkdownObj_quote(next, data);
				else if (next.inner !== undefined)
					parsed += this._processIndentedMrkdownNode(next.inner, data, true);
				//parsed += this['_processMrkdownObj_' + next.inner.id](next.inner, data);

				if (levelData.currentObjIndex + 1 >= levelData.objects.length)
					break;
				next = levelData.objects[levelData.currentObjIndex + 1];
			}
			
		}
		parsed += '</blockquote>';
		return parsed;
	},

	_checkForObject_heading: function (text, index, endIndex, groupLevel, parent, previousLine, whitespace) {
		var startIndex = index + whitespace.total;
		var charCount = this._howMany(text, startIndex, endIndex, '#');
		if (charCount.count === 0 || charCount.count > 6)
			return null;

		if (this._isSpace(text, startIndex + charCount.total) === false)
			return null;

		var obj = new Mrkdwn.markdownLineDetails('heading', charCount.total, charCount.count, false, groupLevel, 'h' + charCount.toString());
		obj.isContainer = false;
		return obj;
	},

	_processMrkdownObj_heading: function (obj, data) {
		var parsed = '<h' + obj.modifierCount + '>';
		var spaceCount = this._howMany(obj.text, obj.lineStart + obj.modifierLength, obj.lineEnd, ' ').count;
		var headingText = obj.text.substring(obj.lineStart + obj.modifierLength + spaceCount, obj.lineEnd);
		parsed += this._pushToNextLevel(headingText, data, { nextIndex: 0, endIndex: headingText.length }, this._checkForMarkdownWordModifiers);
		parsed += '</h' + obj.modifierCount + '>';
		return parsed;
	},

	_checkForObject_headingMod: function (text, index, endIndex, groupLevel, parent, previousLine, whitespace) {

		if (previousLine === null || previousLine.id !== 'text' || previousLine.isEmpty)
			return null;

		var startIndex = index + whitespace.total;
		var hValue = 1;

		var charCount = this._howMany(text, startIndex, endIndex, '=');
		if (charCount.count < 3) {
			charCount = this._howMany(text, startIndex, endIndex, '-');
			if (charCount.count < 3)
				return null;
			else
				hValue = 2;
		}

		if (charCount.hasOther)
			return null;

		previousLine.id = 'heading';
		previousLine.groupSignature = 'h' + hValue.toString();
		previousLine.modifierCount = hValue;
		previousLine.modifierLength = 0;

		return undefined;
	},

	_checkForObject_hLine: function (text, index, endIndex, groupLevel, parent, previousLine, whitespace) {

		var startIndex = index + whitespace.total;
		var charCount = this._howMany(text, startIndex, endIndex, '-');
		if (charCount.count < 3) {
			charCount = this._howMany(text, startIndex, endIndex, '*');
			if (charCount.count < 3) {
				charCount = this._howMany(text, startIndex, endIndex, '_');
				if (charCount.count < 3)
					return null;
			}
		}

		if (charCount.hasOther)
			return null;

		var obj = new Mrkdwn.markdownLineDetails('hLine', charCount.total, charCount.count, false, groupLevel);
		obj.isContainer = false;
		return obj;
	},

	_processMrkdownObj_hLine: function (obj, data) {
		return '<hr>';
	},

	_processMrkdownObj_text: function (obj, data) {
		if (obj.isEmpty)
			return '';

		var levelData = this._getLevelData(data);
		var parsed = '';
		var next = obj.next();
		var hasNewLine = next !== null && next.isEmpty && next.groupLevel === obj.base().groupLevel;
		if (hasNewLine)
			levelData.currentObjIndex += 1;

		var trimSpaces = levelData.trimSpaces === undefined || levelData.trimSpaces === true;

		var tag = obj.isRoot || ((levelData.paragraphOverride === undefined || levelData.paragraphOverride.length === 0) ? hasNewLine : levelData.paragraphOverride[levelData.paragraphOverride.length - 1]);
		if (tag)
			parsed += '<p>';
		var lineText = trimSpaces ? this._trimLeadingSpaces(obj.getLine()) : obj.getLine();
		parsed += this._pushToNextLevel(lineText, data, { nextIndex: 0, endIndex: lineText.length }, this._checkForMarkdownWordModifiers);

		if (levelData.currentObjIndex + 1 < levelData.objects.length) {
			//hard wrapped texted
			if (next.id === 'text') {
				while ((next.groupLevel === obj.base().groupLevel) && (next.isEmpty === undefined || next.isEmpty === false)) {
					levelData.currentObjIndex += 1;
					lineText = this._trimLeadingSpaces(next.getLine());
					parsed += '&nbsp' + this._pushToNextLevel(lineText, data, { nextIndex: 0, endIndex: lineText.length }, this._checkForMarkdownWordModifiers);
					if (levelData.currentObjIndex + 1 >= levelData.objects.length)
						break;
					next = levelData.objects[levelData.currentObjIndex + 1].levelup(next);
				}
			}
		}
		if (tag)
			parsed += '</p>';
		return parsed;
	},


	markdownLineDetails: function (id, modifierLength, modifierCount, hasInner, groupLevel, groupSignature) {
		this.id = id;
		this.hasInner = hasInner;
		this.modifierLength = modifierLength;
		this.modifierCount = modifierCount;
		this.isRoot = groupLevel === 0;
		this.groupSignature = groupSignature === undefined ? id : groupSignature;
		this.groupLevel = groupLevel;

		this.levelup = function (to) {

			var toBase = to.base();
			var toGroup = to.root();
			var group = this;

			while (toGroup.groupLevel < toBase.groupLevel && group.isContainer === true && toGroup.groupSignature === group.groupSignature) {

				if (toGroup.inner === undefined && group.inner === undefined)
					break;

				toGroup = toGroup.inner;
				group = group.inner;
			}

			if (group.groupLevel < toBase.groupLevel) {
				return this;
			}

			return group;
		};

		this.next = function () {
			if (this.nextLine === undefined) {
				return null;
			}

			return this.nextLine.levelup(this);
		};

		this.previous = function () {
			if (this.previousLine === undefined || this.previousLine === null) {
				return null;
			}

			return this.previousLine.levelup(this);
		};

		this.root = function () {
			if (this._rootObject === undefined) {

				if (this.isRoot)
					this._rootObject = this;
				else
					this._rootObject = this.parent.root();
			}

			return this._rootObject;
		};

		this.base = function () {
			if (this._baseObject === undefined) {
				if (this.isRoot || this.isContainer || this.parent.isContainer)
					this._baseObject = this;
				else {
					this._baseObject = this.parent.base();
				}
			}

			return this._baseObject;
		};

		this.setNextLine = function (nextLine) {
			this.nextLine = nextLine;
			if (this.inner !== undefined)
				this.inner.setNextLine(nextLine);
		};

		this.getLine = function (clipModifier) {
			if (this._line === undefined) {
				var offset = 0;
				if (clipModifier)
					offset = this.modifierLength;
				this._line = this.text.substring(this.lineStart + offset, this.lineEnd);
			}

			return this._line;
		};

		this.getModifier = function () {
			if (this.modifier === undefined) {
				this.modifier = this.text.substring(this.lineStart, this.lineStart + this.modifierLength);
			}

			return this.modifier;
		};
	},

	_trimLeadingSpaces: function (text) {
		var i = 0;
		for (i = 0; i < text.length; i++) {
			if (this._isWhiteSpace(text, i) === false)
				break;
		}

		if (i > 0)
			return text.substring(i);

		return text;
	},

	_markdownObjIds: ['multiline_code', 'heading', 'headingMod', 'hLine', 'ul', 'ol', 'quote'],

	_getLineDetails: function (text, lineStart, lineEnd, groupLevel, parent, previousLine) {

		var currentPos = lineStart;
		var whitespace = this._howManyWhiteSpace(text, lineStart, lineEnd);
		var newLine = null;
		if (lineStart < lineEnd && whitespace.hasOther) {

			currentPos += whitespace.total;
		
			if (currentPos < lineEnd) {
				for (var id = 0; id < this._markdownObjIds.length; id++) {
					newLine = this['_checkForObject_' + this._markdownObjIds[id]](text, lineStart, lineEnd, groupLevel, parent, previousLine, whitespace);
					if (newLine !== null)
						break;
				}
			}
		}

		if (newLine !== undefined) {

			if (newLine === null) {
				newLine = new Mrkdwn.markdownLineDetails('text', 0, 0, false, groupLevel);
				newLine.isEmpty = currentPos === lineEnd || !whitespace.hasOther;
				newLine.isContainer = false;
			}

			newLine.lineStart = lineStart;
			newLine.lineEnd = lineEnd;
			newLine.text = text;
			newLine.leadingWhiteSpace = whitespace;
			if (parent !== null) {
				parent.inner = newLine;
				newLine.parent = parent;
			}

			newLine.previousLine = previousLine;
			if (groupLevel === 0 && previousLine !== null)
				previousLine.setNextLine(newLine);

			if (newLine.hasInner)
				this._getLineDetails(text, lineStart + newLine.leadingWhiteSpace.total + newLine.modifierLength, lineEnd, groupLevel + 1, newLine, previousLine);
		}

		return newLine;
	},


	_parseMarkdownWordModifier: function (text, data) {
		var levelData = this._getLevelData(data);
		var targetChar = levelData.typeChar;
		var typeName = levelData.typeName;
		var startIndex = levelData.nextIndex;
		var veryEndIndex = levelData.endIndex;
		var startCount = levelData.typeCount;
		var startCountUsable = startCount;
		var requiresOuterSpace = levelData.requiresOuterSpace;
		var requiresInnerConnection = levelData.requiresInnerConnection;
		var endIndex = startIndex;
		var endCount = 0;
		var parsed = '';

		var next = this._findNextAcceptableEndCommand(text, startIndex + startCount, veryEndIndex, targetChar, requiresOuterSpace, requiresInnerConnection);
		while (next !== -1 && next < veryEndIndex && startCountUsable > 0) {
			var count = this._countCommonChars(text, next, startCountUsable);

			var endCountUsed = count;
			if (!levelData.typeCountSensitive)
				endCountUsed = this._checkWhatsPossible(typeName, count);
			else if (count !== startCount)
				endCountUsed = 0;

			if (endCountUsed > 0) {
				endIndex = next;
				endCount = endCountUsed;
				startCountUsable -= endCountUsed;
			}

			if (startCountUsable > 0) {
				if (endCountUsed === count || endCountUsed === 0)
					next = this._findNextAcceptableEndCommand(text, next + count, veryEndIndex, targetChar, requiresOuterSpace, requiresInnerConnection);
				else
					next += endCountUsed;
			}
		}

		if (endCount > 0) {
			var parseFunctionName = '_processMrkdown_' + typeName;
			if (!levelData.typeCountSensitive)
				parseFunctionName = parseFunctionName + '_' + endCount.toString();
			parsed = this._pushToNextLevel(text, data, { nextIndex: startIndex + endCount, endIndex: endIndex }, this[parseFunctionName])
			//parsed = this[parseFunctionName](text.substring(startIndex + endCount, endIndex), data);
			levelData.nextIndex = endIndex + endCount;
		}
		else {
			parsed = text.substring(startIndex, startIndex + startCount);
			levelData.nextIndex = startIndex + startCount;
		}

		return parsed;
	},

	_checkValidWordModifier: function (text, data) {
		var ignored = false;
		var levelData = this._getLevelData(data);

		var isEscaped = this._isCharEscaped(text, levelData.nextIndex);

		var cmd = text[levelData.nextIndex];
		if (!isEscaped && cmd === '*') {
			ignored = (data.ignore[cmd] !== undefined && data.ignore[cmd] > 0) || this._hasPostspaceFiltered(text, levelData.nextIndex);
			if (!ignored) {
				levelData.typeName = 'asterix';
				levelData.typeChar = '*';
				levelData.typeIgnoreChar = [];
				levelData.typeCount = this._countCommonChars(text, levelData.nextIndex);
				levelData.typeCountSensitive = false;
				levelData.requiresOuterSpace = false;
				levelData.requiresInnerConnection = true;
				levelData.substitutionMethod = false;
			}
		}
		else if (!isEscaped && cmd === '_') {
			ignored = (data.ignore[cmd] !== undefined && data.ignore[cmd] > 0) || !this._hasPrespaceFiltered(text, levelData.nextIndex);
			if (!ignored) {
				levelData.typeName = 'underscore';
				levelData.typeChar = '_';
				levelData.typeIgnoreChar = [];
				levelData.typeCount = this._countCommonChars(text, levelData.nextIndex);
				levelData.typeCountSensitive = false;
				levelData.requiresOuterSpace = true;
				levelData.requiresInnerConnection = true;
				levelData.substitutionMethod = false;
			}
		}
		else if (!isEscaped && cmd === '~') {
			ignored = data.ignore[cmd] !== undefined && data.ignore[cmd] > 0;
			if (!ignored) {
				levelData.typeName = 'tilda';
				levelData.typeChar = '~';
				levelData.typeIgnoreChar = [];
				levelData.typeCount = this._countCommonChars(text, levelData.nextIndex);
				levelData.typeCountSensitive = false;
				levelData.requiresOuterSpace = false;
				levelData.requiresInnerConnection = false;
				levelData.substitutionMethod = false;
			}
		}
		else if (!isEscaped && cmd === '`') {
			ignored = data.ignore[cmd] !== undefined && data.ignore[cmd] > 0;
			if (!ignored) {
				levelData.typeName = 'backtick';
				levelData.typeChar = '`';
				levelData.typeIgnoreChar = ['`', '*', '_', '~'];
				levelData.typeCount = this._countCommonChars(text, levelData.nextIndex);
				levelData.typeCountSensitive = true;
				levelData.requiresOuterSpace = false;
				levelData.requiresInnerConnection = false;
				levelData.substitutionMethod = false;
			}
		}
		else if (!isEscaped && cmd === '-') {
			ignored = data.ignore[cmd] !== undefined && data.ignore[cmd] > 0;
			if (!ignored) {
				levelData.typeName = 'dash';
				levelData.typeChar = '-';
				levelData.typeIgnoreChar = [];
				levelData.typeCount = this._countCommonChars(text, levelData.nextIndex);
				levelData.substitutionMethod = true;
			}
		}
		else if (cmd === '<') {
			var i = 0;
			var endIndex = -1;
			for (i = levelData.nextIndex + 2; i < levelData.nextIndex + 20; i++) {
				if (i >= text.length)
					break;

				var terminator = text[i]
				if (terminator === '<')
					break;
				else if (terminator === '>') {
					endIndex = i;
					break;
				}
			}

			ignored = endIndex === -1;

			if (!ignored) {
				var tag = text.substring(levelData.nextIndex + 1, endIndex);
				if (tag[0] === '/')
					tag = tag.substring(1);
				else if (tag[tag.length - 1] === '/')
					tag = tag.substring(0, tag.length - 1);

				if (this._checkHTMLWhitelist(tag) === false) {
					levelData.typeName = 'blackListedHTMLTag';
					levelData.typeChar = tag;
					levelData.typeIgnoreChar = [];
					levelData.typeCount = 1;
					levelData.substitutionMethod = true;
					levelData.skipTo = endIndex + 1;
				}
				else
					ignored = true;
			}
		}
		else {
			ignored = true;
			levelData.typeName = '';
			levelData.typeChar = cmd;
			levelData.requiresSpace = false;
			levelData.requiresInnerConnection = false;
			levelData.typeIgnoreChar = [];
		}

		levelData.isEscaped = isEscaped;

		return !ignored;
	},

	_checkHTMLWhitelist: function (tag) {
		return tag === 'em' || tag === 'strong' || tag === 'b' || tag === 'i' || tag === 'sup' || tag === 'sub' || tag === 'p' || tag === 'ol' || tag === 'ul' || tag === 'li';
	},

	_checkForMarkdownWordModifiers: function (text, data) {
		var levelData = this._getLevelData(data);
		var parsed = '';
		var textStart = -1;
		var textLength = 0;
		var copyText = function (addlength) {
			if (textStart != -1) {
				addlength = addlength === undefined ? 0 : addlength;
				parsed += text.substring(textStart, textStart + textLength + addlength);
				textStart = -1;
				textLength = 0;
			}
		};

		while (levelData.nextIndex < levelData.endIndex) {
			isPlain = false;
			if (this._checkValidWordModifier(text, data)) {
				copyText();
				if (levelData.substitutionMethod) {
					var subProcessName = '_processMrkdownSub_' + levelData.typeName + '_' + levelData.typeCount;
					if (this[subProcessName] !== undefined)
						parsed += this[subProcessName](text, data);
					else {
						if (textStart === -1)
							textStart = levelData.nextIndex;
						textLength += levelData.typeCount;
					}
					if (levelData.skipTo !== undefined)
						levelData.nextIndex = levelData.skipTo;
					else
						levelData.nextIndex += levelData.typeCount;
				}
				else {
					this._addIgnoreList(data, levelData.typeIgnoreChar);
					parsed += this._parseMarkdownWordModifier(text, data);
					this._removeIgnoreList(data, levelData.typeIgnoreChar);
				}
			}
			else if (this._isLineBreak(text, levelData.nextIndex)) {
				copyText();
				parsed += "<br>";
				levelData.nextIndex += 3;
				lastPos = levelData.nextIndex;
			}
			else {
				if (levelData.isEscaped)
					copyText(-1);
				if (textStart === -1)
					textStart = levelData.nextIndex;
				textLength += 1;
				levelData.nextIndex += 1;
			}
		}

		copyText();

		return parsed
	},

	_processMrkdownSub_blackListedHTMLTag_1: function (text, data) {
		return '';
	},

	_processMrkdownSub_dash_2: function(text, data) {
		return '&ndash;';
	},

	_processMrkdownSub_dash_3: function(text, data) {
		return '&mdash;';
	},

	_processMrkdown_asterix_1: function (text, data) {
		return '<em>' + this._checkForMarkdownWordModifiers(text, data) + '</em>';
	},

	_processMrkdown_asterix_2: function (text, data) {
		return '<strong>' + this._checkForMarkdownWordModifiers(text, data) + '</strong>';
	},

	_processMrkdown_underscore_1: function (text, data) {
		return '<em>' + this._checkForMarkdownWordModifiers(text, data) + '</em>';
	},

	_processMrkdown_underscore_2: function (text, data) {
		return '<strong>' + this._checkForMarkdownWordModifiers(text, data) + '</strong>';
	},

	_processMrkdown_tilda_2: function (text, data) {
		return '<s>' + this._checkForMarkdownWordModifiers(text, data) + '</s>';
	},

	_processMrkdown_backtick: function (text, data) {
		var levelData = this._getLevelData(data);
		return '<code>' + text.substring(levelData.nextIndex, levelData.endIndex).replace(/&/g, '&amp;').replace(/>/g, '&gt;').replace(/</g, '&lt;') + '</code>';
	},

	_processMrkdown_li: function (text, data) {
		return "<li>" + this._checkForMarkdownWordModifiers(text, data) + "</li>";
	},

	_processMrkdown_p: function (text, data) {
		return "<p>" + this._checkForMarkdownWordModifiers(text, data) + "</p>";
	},



	_processHtmlNode: function (node, data) {
		if (node.nodeType === 3) {
			return this._processHtmlText(node, data);
		}
		if (node.nodeType === 1) {
			var tagname = node.nodeName.toLowerCase();
			var process = '_processHtml_' + tagname;
			if (this[process] === undefined)
				return this._processHtml_unknown(node, data);
			else
				return this[process](node, data);
		}
	},

	_getInnerText: function (node, data) {
		var innerText = [];
		var contents = $(node).contents();
		for (var p = 0; p < contents.length; p++) {
			var childNode = contents[p];

			var lineData = this._processHtmlNode(childNode, data);

			if (innerText.length === 0)
				innerText = lineData.lines;
			else if (!lineData.append) {
				if (lineData.lines[lineData.lines.length - 1] === '' && innerText[innerText.length - 1] !== '')
					innerText.push('');
				innerText = innerText.concat(lineData.lines);
			}
			else {
				for (var i = 0; i < lineData.lines.length; i++) {
					if (i === 0)
						innerText[innerText.length - 1] += lineData.lines[i];
					else
						innerText.push(lineData.lines[i]);
				}
			}
		}
		return innerText;
	},

	_processEmphesisWhiteSpace: function (text, emphesis) {
		var leadingSpaces = 0;
		var leadingFinished = false;
		var trailingSpaces = 0;
		var trailingFinished = false;
		for (var i = 0; i < text.length; i++) {
			if (!leadingFinished && this._isWhiteSpace(text, i))
				leadingSpaces += 1;
			else
				leadingFinished = true;

			if (!trailingFinished && this._isWhiteSpace(text, text.length - i - 1))
				trailingSpaces += 1;
			else
				trailingFinished = true;

			if (leadingFinished && trailingFinished)
				break;
		}

		var returnText;
		if (leadingSpaces > 0 && trailingSpaces > 0) {

			returnText = text.substring(0, leadingSpaces);
			returnText += emphesis;
			returnText += text.substring(leadingSpaces, text.length - trailingSpaces);
			returnText += emphesis;
			returnText += text.substring(text.length - trailingSpaces, text.length);
		}
		else if (leadingSpaces > 0) {

			returnText = text.substring(0, leadingSpaces);
			returnText += emphesis;
			returnText += text.substring(leadingSpaces, text.length);
			returnText += emphesis;
		}
		else if (trailingSpaces > 0) {

			returnText = emphesis;
			returnText += text.substring(0, text.length - trailingSpaces);
			returnText += emphesis;
			returnText += text.substring(text.length - trailingSpaces, text.length);
		}
		else
			returnText = emphesis + text + emphesis;

		return returnText;
	},

	_escapeTextForMarkdown: function (text) {
		var startIndex = 0;
		var escapedText = '';
		var chars = ['\\', '`', '*', '_', '{', '}', '[', ']', '(', ')', '#', '.', '!', '+', '-'];
		for (var i = 0; i < text.length; i++) {
			for (var c = 0; c < text.length; c++) {
				if (text[i] === chars[c]) {
					escapedText += text.substring(startIndex, i) + '\\';
					startIndex = i;
					break;
				}
			}
		}

		if (startIndex === 0)
			return text;
		else if (startIndex < text.length)
			escapedText += text.substring(startIndex, i);
		
		return escapedText;
	},


	_processHtml_p: function (node, data) {
		data.inParagraph = true;
		var lines = this._pushToNextLevel(node, data, null, this._getInnerText);
		lines.push('');
		data.inParagraph = false;
		return { append: false, lines: lines };
	},

	_processHtml_h1: function (node, data) {
		var lines = ['# ' + this._pushToNextLevel(node, data, null, this._getInnerText), ''];
		return { append: false, lines: lines };
	},

	_processHtml_h2: function (node, data) {
		var lines = ['## ' + this._pushToNextLevel(node, data, null, this._getInnerText), ''];
		return { append: false, lines: lines };
	},

	_processHtml_h3: function (node, data) {
		var lines = ['### ' + this._pushToNextLevel(node, data, null, this._getInnerText), ''];
		return { append: false, lines: lines };
	},

	_processHtml_h4: function (node, data) {
		var lines = ['#### ' + this._pushToNextLevel(node, data, null, this._getInnerText), ''];
		return { append: false, lines: lines };
	},

	_processHtml_h5: function (node, data) {
		var lines = ['##### ' + this._pushToNextLevel(node, data, null, this._getInnerText), ''];
		return { append: false, lines: lines };
	},

	_processHtml_h6: function (node, data) {
		var lines = ['###### ' + this._pushToNextLevel(node, data, null, this._getInnerText), ''];
		return { append: false, lines: lines };
	},

	_processHtml_hr: function (node, data) {
		var lines = ['----', ''];
		return { append: false, lines: lines };
	},

	_processHtml_code: function (node, data) {
		data.inCode = true;
		var lines = this._pushToNextLevel(node, data, null, this._getInnerText);
		if (data.inPre) {
			for (var i = 0; i < lines.length; i++)
				lines[i] = "\t" + lines[i];

			lines.push('');
		}
		else
			lines = ["`" + lines.join("\n") + "`"];

		data.inCode = false;

		return { append: !data.inPre, lines: lines };
	},

	_processHtml_pre: function (node, data) {
		data.inPre = true;
		var lines = this._pushToNextLevel(node, data, null, this._getInnerText);
		data.inPre = false;
		return { append: false, lines: lines };
	},

	_processHtml_ul: function (node, data) {

		var lines = this._pushToNextLevel(node, data, { listType: 'ul', indent: '\t' }, this._getInnerText);

		var levelData = this._getLevelData(data);
		if (levelData !== null && levelData.topLevel)
			lines.push('');

		return { append: false, lines: lines };
	},

	_processHtml_ol: function (node, data) {

		var lines = this._pushToNextLevel(node, data, { listType: 'ol', rowIndex: 1, indent: '\t' }, this._getInnerText)

		var levelData = this._getLevelData(data);
		if (levelData!== null && levelData.topLevel)
			lines.push('');

		return { append: false, lines: lines };
	},

	_processHtml_blockquote: function (node, data) {

		var lines = this._pushToNextLevel(node, data, null, this._getInnerText);

		for (var i = 0; i < lines.length; i++)
			lines[i] = '> ' + lines[i];

		lines.push('');

		return { append: false, lines: lines };
	},

	_processHtml_li: function (node, data) {
		var levelData = this._getLevelData(data);
		var lines = this._pushToNextLevel(node, data, null, this._getInnerText);

		for (var i = 0; i < lines.length; i++) {
			if (i === 0) {
				if (levelData.listType === 'ul')
					lines[i] = '* ' + lines[i];
				else if (levelData.listType === 'ol') {
					var index = levelData.rowIndex;
					levelData.rowIndex += 1;
					lines[i] = index.toString() + '. ' + lines[i];
				}
			}
			else if (lines[i] !== '')
				lines[i] = levelData.indent + lines[i];
		}
		return { append: false, lines: lines };
	},

	_processHtmlText: function (node, data) {
		var lines = node.textContent;
		if (!data.inCode)
			lines = this._escapeTextForMarkdown(lines);
		lines = lines.split('\n');
		return { append: true, lines: lines }
	},

	_processHtml_unknown: function (node, data) {

		return { append: false, lines: [ node.outerHTML ] };
	},

	_processHtml_sup: function (node, data) {

		var lines = this._pushToNextLevel(node, data, null, this._getInnerText);
		lines = ['<sup>' + lines.join("\n") + '</sup>'];
		return { append: true, lines: lines }
	},

	_processHtml_sub: function (node, data) {

		var lines = this._pushToNextLevel(node, data, null, this._getInnerText);
		lines = ['<sub>' + lines.join("\n") + '</sub>'];
		return { append: true, lines: lines }
	},

	_processHtml_underline: function (node, data) {

		var lines = this._pushToNextLevel(node, data, null, this._getInnerText);
		lines = ['<underline>' + lines.join("\n") + '</underline>'];
		return { append: true, lines: lines }
	},

	_processHtml_strong: function (node, data) {
		var lines = this._pushToNextLevel(node, data, null, this._getInnerText);
		lines = [ this._processEmphesisWhiteSpace(lines.join("\n"), '**') ];
		return { append: true, lines: lines }
	},

	_processHtml_em: function (node, data) {
		var lines = this._pushToNextLevel(node, data, null, this._getInnerText);
		lines = [this._processEmphesisWhiteSpace(lines.join("\n"), '*')];
		return { append: true, lines: lines }
	},

	_processHtml_s: function (node, data) {
		var lines = this._pushToNextLevel(node, data, null, this._getInnerText);
		lines = ["~~" + lines.join("\n") + "~~"];
		return { append: true, lines: lines }
	},

	_processHtml_i: function (node, data) {
		var lines = this._pushToNextLevel(node, data, null, this._getInnerText);
		lines = [this._processEmphesisWhiteSpace(lines.join("\n"), '*')];
		return { append: true, lines: lines }
	},

	_processHtml_b: function (node, data) {
		var lines = this._pushToNextLevel(node, data, null, this._getInnerText);
		lines = [this._processEmphesisWhiteSpace(lines.join("\n"), '**')];
		return { append: true, lines: lines }
	},

	_processHtml_br: function (node, data) {
		var lines = ["  \n"];
		return { append: true, lines: lines }
	},
}
