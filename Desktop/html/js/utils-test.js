var utilsTestResults = [];
var utilsTestPassed = 0;
var utilsTestFailed = 0;

function _reset() {
	setCurrentLocaleID('en', true);
	window.globSet.decimals = "";
	window.globSet.pExact = false;
	window.globSet.normalizedNotation = true;
}

function _assertEq(actual, expected, name) {
	var passed = actual === expected;
	var result = { test: name, passed: passed, actual: actual, expected: expected };
	if (passed) utilsTestPassed++; else utilsTestFailed++;
	utilsTestResults.push(result);
}

function _colVals(vals) {
	return vals.map(function(v) { return { content: v }; });
}

function _fmtCol(vals, type, format, html, combine, footnotes) {
	if (html === undefined) html = true;
	if (combine === undefined) combine = false;
	if (footnotes === undefined) footnotes = null;
	return formatColumn(_colVals(vals), type, format, false, combine, footnotes, html);
}

function _fmtOne(val, type, format, html) {
	return _fmtCol([val], type, format, html)[0].content;
}

var _TS = "&thinsp;";
var _MIN = "&minus;";

// ========== formatFixed ==========
function test_formatFixed() {
	_reset();
	var t = _assertEq;

	t(formatFixed(1.234, 2), "1.23", "formatFixed: 1.234 to 2 decimals");
	t(formatFixed(1.5, 3), "1.500", "formatFixed: 1.5 to 3 decimals pads zeros");
	t(formatFixed(0, 2), "0.00", "formatFixed: zero to 2 decimals");
	t(formatFixed(0, 0), "0", "formatFixed: zero to 0 decimals");
	t(formatFixed(-5.678, 3), "-5.678", "formatFixed: negative to 3 decimals");
	t(formatFixed(1.999, 2), "2.00", "formatFixed: rounding up to 2 decimals");
	t(formatFixed(1.2349, 3), "1.235", "formatFixed: round up last digit");
	t(formatFixed(9.999, 2), "10.00", "formatFixed: rounding carries over decimal");
	t(formatFixed(999.9999, 0), "1000", "formatFixed: rounding to 0 decimals");

	t(formatFixed(12345.678, 2, false, true), "12,345.68", "formatFixed: with thousands grouping");
	t(formatFixed(123456789.1, 1, false, true), "123,456,789.1", "formatFixed: large number grouping");
	t(formatFixed(12345.678, 2, false, false), "12345.68", "formatFixed: without thousands grouping");

	t(formatFixed(0.0001, 4), "0.0001", "formatFixed: small number 4 decimals");
	t(formatFixed(0.001, 3), "0.001", "formatFixed: exactly 3 decimals");
	t(formatFixed(0.25, 4), "0.2500", "formatFixed: fractional pads zeros");

	t(formatFixed(0.5, 2, true), ".50", "formatFixed: noZeroLead strips leading 0");
	t(formatFixed(0.05, 3, true), ".050", "formatFixed: noZeroLead .05");
	t(formatFixed(0.9999, 2, true), "1.00", "formatFixed: noZeroLead with rounding to 1 leaves 1");
	t(formatFixed(0.999, 0, true), "1", "formatFixed: noZeroLead rounding to 1 at 0 decimals");
	t(formatFixed(1.5, 2, true), "1.50", "formatFixed: noZeroLead no effect for >= 1");

	t(formatFixed(1.5, NaN), "2", "formatFixed: NaN digits defaults to 0");

	t(formatFixed(Infinity, 2), "∞", "formatFixed: Infinity");
	t(formatFixed(-Infinity, 2), "-∞", "formatFixed: -Infinity");
}

// ========== formatPrecision ==========
function test_formatPrecision() {
	_reset();
	var t = _assertEq;

	t(formatPrecision(1.234, 2), "1.2", "formatPrecision: 2 sig digits");
	t(formatPrecision(9.876, 3), "9.88", "formatPrecision: 3 sig digits rounds");
	t(formatPrecision(12345, 4), "12350", "formatPrecision: large, 4 sig digits");
	t(formatPrecision(12345, 4, false, true), "12,350", "formatPrecision: large with thousands");
	t(formatPrecision(0.001234, 2), "0.0012", "formatPrecision: very small 2 sig");
	t(formatPrecision(0.001234, 3), "0.00123", "formatPrecision: very small 3 sig");
	t(formatPrecision(999.9, 4), "999.9", "formatPrecision: boundary at 4 sig");
	t(formatPrecision(999.99, 3), "1000", "formatPrecision: rounding up across boundary");
	t(formatPrecision(1e-7, 2), "0.00000010", "formatPrecision: extremely small");
	t(formatPrecision(1e8, 3), "100000000", "formatPrecision: extremely large");
	t(formatPrecision(0.5, 2, true), ".50", "formatPrecision: noZeroLead strips leading 0");
	t(formatPrecision(0.0999, 2, true), ".10", "formatPrecision: noZeroLead with rounding-up");
}

// ========== formatPrecisionWithRespectForFixedDecimals ==========
function test_formatPrecisionWRFD() {
	_reset();
	var f = formatPrecisionWithRespectForFixedDecimals;
	var t = _assertEq;

	t(f(1.23456, 3, 4), "1.23", "formatPrecisionWRFD: sf=3 dp=4, sf wins");
	t(f(100.456, 3, 5), "100", "formatPrecisionWRFD: sf=3 dp=5, sf wins on large");
	t(f(0.1234, 5, 2), "0.12", "formatPrecisionWRFD: sf=5 dp=2, dp wins");
	t(f(12.3456, 6, 3), "12.346", "formatPrecisionWRFD: sf=6 dp=3 balanced");
	t(f(0.05, 4, 3), "0.050", "formatPrecisionWRFD: sf=4 dp=3");
	t(f(0.05, 4, 3, true), ".050", "formatPrecisionWRFD: sf=4 dp=3 noZeroLead");
	t(f(123.456, 4, NaN), "123.5", "formatPrecisionWRFD: NaN dp uses precision only");
}

// ========== formatMoney ==========
function test_formatMoney() {
	_reset();
	var t = _assertEq;

	setCurrentLocaleID('en', true);
	t(formatMoney('EUR', 1234.56), "€1,234.56", "formatMoney: EUR with decimals");
	t(formatMoney('EUR', 1000), "€1,000", "formatMoney: EUR integer strips trailing zeros");
	t(formatMoney('EUR', 0), "€0", "formatMoney: EUR zero strips trailing zeros");
	t(formatMoney('EUR', 1234567.89), "€1,234,567.89", "formatMoney: EUR large with grouping");
	t(formatMoney('USD', 1234.56), "$1,234.56", "formatMoney: USD");
	t(formatMoney('EUR', 1234.56, false), "€1,234.56", "formatMoney: EUR thousands=false but global true overrides");

	setCurrentLocaleID('en', false);
	t(formatMoney('EUR', 1234, false), "€1234", "formatMoney: EUR no grouping when both off");
	t(formatMoney('EUR', 1234, true), "€1,234", "formatMoney: EUR thousands=true overrides global off");
	t(formatMoney('EUR', 0, true), "€0", "formatMoney: EUR zero returns €0");
}

// ========== formatNumber ==========
function test_formatNumber() {
	_reset();
	var t = _assertEq;

	t(formatNumber(1234567), "1234567", "formatNumber: default no grouping");
	t(formatNumber(1234567, true), "1,234,567", "formatNumber: thousands=true");
	t(formatNumber(-1234.56, true), "-1,234.56", "formatNumber: negative with grouping");
	t(formatNumber(0), "0", "formatNumber: zero");
	t(formatNumber(0.12345), "0.123", "formatNumber: small decimal (default digits)");
}

// ========== handleNoZeroLead ==========
function test_handleNoZeroLead() {
	_reset();
	var t = _assertEq;

	t(handleNoZeroLead(0.5, "0.50", true), ".50", "handleNoZeroLead: strips leading zero");
	t(handleNoZeroLead(0.05, "0.05", true), ".05", "handleNoZeroLead: .05");
	t(handleNoZeroLead(0, "0.00", true), ".00", "handleNoZeroLead: zero");
	t(handleNoZeroLead(1.5, "1.50", true), "1.50", "handleNoZeroLead: >=1 no strip");
	t(handleNoZeroLead(-0.5, "-0.50", true), "-0.50", "handleNoZeroLead: negative no strip");
	t(handleNoZeroLead(0.5, "0.50", false), "0.50", "handleNoZeroLead: flag off no strip");
	t(handleNoZeroLead(0.9999, "1.00", true), "1.00", "handleNoZeroLead: rounded to 1 starts with 1 no strip");
}

// ========== toExponential ==========
function test_toExponential() {
	_reset();
	var t = _assertEq;

	window.globSet.normalizedNotation = true;
	t(toExponential(123456, 3, 0, true), "1.235&times;10<sup>+5</sup>", "toExponential: normalized html positive");
	t(toExponential(0.000123, 2, 0, true), "1.23&times;10<sup>" + _MIN + "4</sup>", "toExponential: normalized html negative exp");
	t(toExponential(1.5, 3, 0, true), "1.500&times;10<sup>+0</sup>", "toExponential: zero exponent");

	window.globSet.normalizedNotation = false;
	t(toExponential(123456, 3, 0, true), "1.235e+5", "toExponential: non-normalized html");
	t(toExponential(0.000123, 2, 0, false), "1.23e-4", "toExponential: non-normalized non-html");

	window.globSet.normalizedNotation = true;
	t(toExponential(-1234, 2, 0, true), _MIN + "1.23&times;10<sup>+3</sup>", "toExponential: negative number html");
	t(toExponential(-5.678, 2, 0, true), _MIN + "5.68&times;10<sup>+0</sup>", "toExponential: negative small absolute value");
}

// ========== formatCellforLaTeX ==========
function test_formatCellforLaTeX() {
	_reset();
	var t = _assertEq;

	t(formatCellforLaTeX('&nbsp;'), '', "formatCellforLaTeX: nbsp to empty");
	t(formatCellforLaTeX('42'), '$42$', "formatCellforLaTeX: number in math block");
	t(formatCellforLaTeX('hello'), 'hello', "formatCellforLaTeX: plain text passes through");
	t(formatCellforLaTeX('a<b'), 'a$<$b', "formatCellforLaTeX: less-than sign wrapped");
	t(formatCellforLaTeX('x<sup>2</sup>'), 'x$^{2}$', "formatCellforLaTeX: superscript handled");
	t(formatCellforLaTeX('H<sub>0</sub>'), 'H$_{0}$', "formatCellforLaTeX: subscript handled");
	t(formatCellforLaTeX('<em>text</em>'), '\\textit{text}', "formatCellforLaTeX: emphasis");
	t(formatCellforLaTeX('_underscore'), '\\_underscore', "formatCellforLaTeX: underscore escaped");
}

// ========== formatColumn: string type and null format ==========
function test_formatColumn_string() {
	_reset();
	var f = _fmtCol;
	var t = _assertEq;

	t(f(["hello"], "string", null)[0].content, "hello", "fmtCol: string type pass-through");
	t(f(["hello"], "string", null)[0]["class"], "text", "fmtCol: string type class text");
	t(f(["johndoe"], null, null)[0].content, "johndoe", "fmtCol: null type, null format pass-through");
	t(f(["a\u273Bb"], "string", null)[0].content, "a<small>\u273B</small>b", "fmtCol: string type asterisk wrapped in small");

	// combine
	var cells = _fmtCol(["a", "a", "b"], "string", null, true, true);
	t(cells[0].content, "a", "fmtCol: combine first occurrence shown");
	t(cells[1].content, "&nbsp;", "fmtCol: combine duplicate collapsed to nbsp");
	t(cells[2].content, "b", "fmtCol: combine different value shown");

	cells = _fmtCol(["a", "a"], "string", null, false, true);
	t(cells[0].content, "a", "fmtCol: combine html=false first");
	t(cells[1].content, " ", "fmtCol: combine html=false collapsed to space");
}

// ========== formatColumn: significance (sf) ==========
// Note: sf:N with dp:M uses minLSD for decimal alignment, NOT sig-fig rounding
function test_formatColumn_sigFig() {
	_reset();
	var fm = _fmtOne;
	var fc = _fmtCol;
	var t = _assertEq;

	// Column alignment with sf:4;dp:3 — mixed scales share the same minLSD=-3
	// sf:4;dp:3 with !fixDecimals: values with abs >= 10^sf go exponential, others use formatPrecision(sf)
	var cells = fc([15606, 1.234, 0.00123, -0.5], "number", "sf:4;dp:3");
	t(cells[0].content, "1.561&times;10<sup>+4</sup>", "fmtCol sf: sf:4;dp:3 large integer exponential (>=10^sf)");
	t(cells[1].content, "1.234", "fmtCol sf: sf:4;dp:3 normal decimal precision");
	t(cells[2].content, "0.001230", "fmtCol sf: sf:4;dp:3 very small sig-fig preserved");
	t(cells[3].content, _MIN + "0.5000", "fmtCol sf: sf:4;dp:3 negative sig-fig expanded");
	t(cells[0]["class"], "number", "fmtCol sf: class number");

	// Single-cell sf:4;dp:3 — minLSD computed from one value only
	t(fm(0.5, "number", "sf:4;dp:3"), "0.5000", "fmtCol sf: sf:4;dp:3 single cell sig-fig expanded");

	// Zero
	t(fm(0, "number", "sf:4;dp:3"), "0.000", "fmtCol sf: zero sf:4;dp:3");
	t(fm(0, "number", "sf:3;dp:1"), "0.0", "fmtCol sf: zero sf:3;dp:1");

	// Large number -> exponential
	t(fm(123456789, "number", "sf:3;dp:3"), "1.23&times;10<sup>+8</sup>", "fmtCol sf: large number exponential");

	// Very small number -> exponential
	t(fm(0.00005, "number", "sf:4;dp:3"), "5.000&times;10<sup>" + _MIN + "5</sup>", "fmtCol sf: very small exponential");

	// Thousands token enabled
	t(fm(12345.678, "number", "sf:4;dp:3;thousands"), "1.235&times;10<sup>+4</sup>", "fmtCol sf: thousands in format (>=10^sf → exponential)");

	// fixDecimals=true (globSet.decimals set)
	window.globSet.decimals = 3;
	t(fm(15606, "number", "sf:4"), "15606.000", "fmtCol sf: fixDecimals forces dp alignment");
	t(fm(1.234, "number", "sf:4"), "1.234", "fmtCol sf: fixDecimals normal value");
	window.globSet.decimals = "";

	// Negative minus replacement
	t(fm(-1.5, "number", "sf:4;dp:3"), _MIN + "1.500", "fmtCol sf: negative html has &minus; (sig-fig pads)");

	// Non-number content
	t(fc(["abc"], "number", "sf:4;dp:3")[0].content, "abc", "fmtCol sf: non-number string passes through");
	t(fc(["abc"], "number", "sf:4;dp:3")[0]["class"], "number", "fmtCol sf: non-number gets number class");

	// Various scales from debug.csv data
	t(fm(8.38e98, "number", "sf:3;dp:3"), "8.38&times;10<sup>+98</sup>", "fmtCol sf: 8.38e98 exponential");
	t(fm(1.7e-20, "number", "sf:3;dp:3"), "1.70&times;10<sup>" + _MIN + "20</sup>", "fmtCol sf: 1.7e-20 exponential");
	t(fm(18858693.53, "number", "sf:3;dp:3"), "1.89&times;10<sup>+7</sup>", "fmtCol sf: 18858693.53 -> exponential");
	t(fm(0.99122228, "number", "sf:4;dp:3"), "0.9912", "fmtCol sf: 0.99122228 4sf");
	t(fm(-1.953344972, "number", "sf:4;dp:3"), _MIN + "1.953", "fmtCol sf: -1.953344972 4sf sig-fig");
	t(fm(3.356094448, "number", "sf:4;dp:3"), "3.356", "fmtCol sf: 3.356094448 4sf");
	t(fm(-9.96e-100, "number", "sf:3;dp:3"), _MIN + "9.96&times;10<sup>" + _MIN + "100</sup>", "fmtCol sf: -9.96e-100");
	t(fm(1.28e-100, "number", "sf:3;dp:3"), "1.28&times;10<sup>" + _MIN + "100</sup>", "fmtCol sf: 1.28e-100");
	t(fm(0.008650772, "number", "sf:3;dp:3"), "0.00865", "fmtCol sf: 0.008650772 sf sig-fig");
	t(fm(0.042666202, "number", "sf:3;dp:3"), "0.0427", "fmtCol sf: 0.042666202 sf sig-fig");
}

// ========== formatColumn: decimalPoints (dp) ==========
function test_formatColumn_decimals() {
	_reset();
	var fm = _fmtOne;
	var t = _assertEq;

	t(fm(1.234, "number", "dp:3"), "1.234", "fmtCol dp: 3 decimals");
	t(fm(1.5, "number", "dp:3"), "1.500", "fmtCol dp: pads to 3 decimals");
	t(fm(1.999, "number", "dp:2"), "2.00", "fmtCol dp: rounding 2 decimals");
	t(fm(-5.678, "number", "dp:2"), _MIN + "5.68", "fmtCol dp: negative value uses &minus;");
	t(fm(15606, "number", "dp:3"), "15606.000", "fmtCol dp: integer with 3 decimals");
	t(fm(15606, "number", "dp:3", false), "15606.000", "fmtCol dp: 15606 dp:3 html=false");
	t(fm(0.0005, "number", "dp:3"), "0.001", "fmtCol dp: very small rounds to 0.001");
	t(fm(0, "number", "dp:2"), "0.00", "fmtCol dp: zero 2 decimals");
	t(fm(1234567.89, "number", "dp:1;thousands"), "1,234,567.9", "fmtCol dp: large 1 decimal thousands enabled");

	// Very small number with p: threshold — p is set so small values show threshold
	// 0.0005 < 0.01, shows actual p threshold → "< .0100" with noZeroLead
	t(fm(0.0005, "number", "p:0.01;dp:4"), "<&nbsp;.0100", "fmtCol dp: very small with p threshold");
}

// ========== formatColumn: percentage (pc) ==========
function test_formatColumn_percentage() {
	_reset();
	var fm = _fmtOne;
	var t = _assertEq;

	t(fm(0.5, "number", "pc"), "50.00" + _TS + "%", "fmtCol pc: 50% basic");
	t(fm(0.01, "number", "pc"), "1.00" + _TS + "%", "fmtCol pc: 1%");
	t(fm(0.9999, "number", "pc"), "99.99" + _TS + "%", "fmtCol pc: 99.99% near 100");
	t(fm(1.01, "number", "pc"), "101.00" + _TS + "%", "fmtCol pc: 101% over 100");
	t(fm(0.001, "number", "pc"), "0.10" + _TS + "%", "fmtCol pc: 0.1% small");
	t(fm(0.1234, "number", "pc:3"), "12.340" + _TS + "%", "fmtCol pc: 12.34% 3dp");
	t(fm(0.5, "number", "pc:0"), "50" + _TS + "%", "fmtCol pc: 50% 0dp");
	t(fm(15.0, "number", "pc;thousands"), "1,500.00" + _TS + "%", "fmtCol pc: 1500% with thousands");
	t(fm(0, "number", "pc"), "0.00" + _TS + "%", "fmtCol pc: 0%");
}

// ========== formatColumn: monetary ==========
function test_formatColumn_monetary() {
	_reset();
	var fm = _fmtOne;
	var t = _assertEq;

	setCurrentLocaleID('en', true);
	t(fm(1234.56, "number", "monetary:EUR"), "€1,234.56", "fmtCol monetary: EUR with decimal");
	t(fm(1000, "number", "monetary:EUR"), "€1,000", "fmtCol monetary: EUR integer");
	t(fm(1000234, "number", "monetary:EUR"), "€1,000,234", "fmtCol monetary: EUR large");

	setCurrentLocaleID('en', false);
	t(fm(1000234, "number", "monetary:EUR;thousands"), "€1,000,234", "fmtCol monetary: EUR thousands overrides global false");
	t(fm(1234, "number", "monetary:USD"), "$1234", "fmtCol monetary: USD with grouping off");

	var col = _fmtCol([1234], "number", "monetary:EUR");
	t(col[0]["class"], "monetary", "fmtCol monetary: class monetary");
}

// ========== formatColumn: p-value ==========
function test_formatColumn_pvalue() {
	_reset();
	var fm = _fmtOne;
	var fc = _fmtCol;
	var t = _assertEq;

	// dp-based p:0.01;dp:3 — decimalPoints path
	t(fm(0.9999, "number", "p:0.01;dp:3"), "1.000", "fmtCol pval dp: >=p formatted normally");
	t(fm(0.9, "number", "p:0.01;dp:3"), ".900", "fmtCol pval dp: >=p with noZeroLead");

	// < p threshold — shows actual p value with noZeroLead
	t(fm(0.005, "number", "p:0.01;dp:3"), "<&nbsp;.010", "fmtCol pval dp: < p threshold shows p");
	t(fm(0.0005, "number", "p:0.001;dp:3"), "<&nbsp;.001", "fmtCol pval dp: < p threshold p:0.001");
	t(fm(0.042, "number", "p:0.05;dp:3"), "<&nbsp;.050", "fmtCol pval dp: < .05 threshold shows .050");
	t(fm(0.12, "number", "p:0.05;dp:3"), ".120", "fmtCol pval dp: >= .05 no zero lead");

	// pExact=true forces sf=4 → sf-based formatting with exponential
	window.globSet.pExact = true;
	t(fm(0.05, "number", "p:0.01;dp:3"), "5.000&times;10<sup>" + _MIN + "2</sup>", "fmtCol pval: pExact forces exponential");
	window.globSet.pExact = false;

	// pExact + fixDecimals → also exponential
	window.globSet.pExact = true;
	window.globSet.decimals = 3;
	t(fm(0.05, "number", "p:0.01;dp:3"), "5.000&times;10<sup>" + _MIN + "2</sup>", "fmtCol pval: pExact+fixDecimals exponential");
	window.globSet.pExact = false;
	window.globSet.decimals = "";

	// sf-based p-value: sf:4 → significance path
	// 0.005 < p=0.01 → threshold display using formatPrecisionWRFD(0.01, 4, noZeroLead=true, ...)
	// noZeroLead gets passed as dp param → treated as dp=1 → "0.0"
	t(fm(0.005, "number", "p:0.01;sf:4;dp:3"), "<&nbsp;0.0", "fmtCol pval sf: < p threshold with sf");
	t(fm(0.05, "number", "p:0.01;sf:4;dp:3"), ".050", "fmtCol pval sf: >= p significance");
	// 0 < p=0.01 → goes to < p branch (check comes before ==0 check)
	t(fm(0, "number", "p:0.01;sf:4;dp:3"), "<&nbsp;0.0", "fmtCol pval sf: zero < p shows threshold");

	// p-value class
	t(fc([0.005], "number", "p:0.01;dp:3")[0]["class"], "p-value", "fmtCol pval: class p-value");
}

// ========== formatColumn: mixed type ==========
function test_formatColumn_mixed() {
	_reset();
	var t = _assertEq;

	var col = [{
		content: { value: 1.234, type: "number", format: "sf:4" }
	}, {
		content: { value: "hello", type: "string", format: null }
	}, {
		content: { value: 0.5, type: "number", format: "pc" }
	}];
	var result = formatColumn(col, "mixed", null, false, false, null, true);
	t(result.length, 3, "fmtCol mixed: returns correct length");
	t(result[2].content, "50.00" + _TS + "%", "fmtCol mixed: sub-percentage formatted");

	var nested = [{
		content: { value: { value: 1, type: "number", format: "dp:2" }, type: "mixed", format: null }
	}];
	t(formatColumn(nested, "mixed", null, false, false, null, true, true), "Error: nested mixed columns are not supported!", "fmtCol mixed: nested mixed error");
}

// ========== formatColumn: edge cases ==========
function test_formatColumn_edges() {
	_reset();
	var fm = _fmtOne;
	var fc = _fmtCol;
	var t = _assertEq;

	t(fm(undefined, "number", "dp:2"), ".", "fmtCol edge: undefined -> dot");
	t(fm(undefined, "string", null), ".", "fmtCol edge: undefined string -> dot");

	t(fm("", "number", "dp:2"), "&nbsp;", "fmtCol edge: empty string number -> nbsp");
	// Empty string in string type: parsed, combine check, falls to !isNumber → passes through as ""
	t(fm("", "string", null), "", "fmtCol edge: empty string string type passes through");

	// Approx (~)
	t(fm(1.5, "number", "dp:2;~"), "~" + _TS + "1.50", "fmtCol edge: approx dp");
	t(fm(1.5, "number", "sf:4;dp:3;~"), "~" + _TS + "1.500", "fmtCol edge: approx sf sig-fig");

	// Group markers
	var colWithGroups = [
		{ content: 1.0, isStartOfGroup: true },
		{ content: 2.0 },
		{ content: 3.0, isEndOfGroup: true }
	];
	var res = formatColumn(colWithGroups, "number", "dp:1", false, false, null, true);
	t(res[0]["class"].indexOf("new-group-row") !== -1, true, "fmtCol edge: start of group class");
	t(res[2]["class"].indexOf("last-group-row") !== -1, true, "fmtCol edge: end of group class");

	var colWithSub = [{ content: 1.0, isStartOfSubGroup: true }];
	res = formatColumn(colWithSub, "number", "dp:1", false, false, null, true);
	t(res[0]["class"].indexOf("new-sub-group-row") !== -1, true, "fmtCol edge: sub group class");

	// Footnotes
	var colWithFootnotes = [{ content: 1.23, footnotes: [0] }];
	res = formatColumn(colWithFootnotes, "number", "dp:2", false, false, ["my footnote"], true);
	t(res[0].footnotes[0], "ᵃ", "fmtCol edge: footnote symbol attached");

	// html=false (LaTeX path)
	t(fm(1.234, "number", "dp:2", false), "1.23", "fmtCol edge: html=false basic");
	// Single-cell sf:4;dp:3 html=false: 15606 >= 10^4 → exponential
	t(fm(15606, "number", "sf:4;dp:3", false), "1.561×10<sup>+4</sup>", "fmtCol edge: html=false sf exponential");
	t(fm(0.5, "number", "pc", false), "50.00%", "fmtCol edge: pc html=false");
// 0.005 < 0.01 → shows actual p threshold
	t(fm(0.005, "number", "p:0.01;dp:3", false), "< .010", "fmtCol edge: p-value html=false");

	// NaN content — non-number cell falls through to non-number branch, content = NaN (or null)
	var nanCol = _fmtCol([NaN], "number", "dp:2");
	t(nanCol[0].content === null || nanCol[0].content === "NaN" || typeof nanCol[0].content === "number", true, "fmtCol edge: NaN content handled");
}

// ========== Run all tests ==========
function runAllUtilTests() {
	utilsTestResults = [];
	utilsTestPassed = 0;
	utilsTestFailed = 0;

	test_formatFixed();
	test_formatPrecision();
	test_formatPrecisionWRFD();
	test_formatMoney();
	test_formatNumber();
	test_handleNoZeroLead();
	test_toExponential();
	test_formatCellforLaTeX();
	test_formatColumn_string();
	test_formatColumn_sigFig();
	test_formatColumn_decimals();
	test_formatColumn_percentage();
	test_formatColumn_monetary();
	test_formatColumn_pvalue();
	test_formatColumn_mixed();
	test_formatColumn_edges();

	return {
		passed: utilsTestPassed,
		failed: utilsTestFailed,
		total: utilsTestPassed + utilsTestFailed,
		results: utilsTestResults
	};
}