//
// Copyright (C) 2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "testcolumnencodercontext.h"

#include "columnencodercontext.h"

#include <QtTest>

#include <functional>
#include <initializer_list>
#include <stdexcept>

namespace
{
constexpr const char * ExtraOptionsPrefix = "JaspExtraOptions_";

ColumnEncoder::colTypeMap emptyNames()
{
	return ColumnEncoder::colTypeMap();
}

ColumnEncoder::colTypeMap names(std::initializer_list<std::pair<std::string, columnType>> values)
{
	ColumnEncoder::colTypeMap result;
	for(const auto & value : values)
		result[value.first] = value.second;
	return result;
}

ColumnEncoderContext context(ColumnEncoder::colTypeMap columns, ColumnEncoder::colTypeMap extra)
{
	return ColumnEncoderContext(columns, extra);
}

std::string contextJson(const ColumnEncoderContext & encoderContext)
{
	return encoderContext.toJson().toStyledString();
}

void activateContext(const ColumnEncoderContext & encoderContext, ColumnEncoder & extraEncoder)
{
	ColumnEncoder::columnEncoder()->setCurrentNames(encoderContext.columns());
	extraEncoder.setCurrentNames(encoderContext.extra());
}

std::string encodeColumn(const ColumnEncoderContext & encoderContext, ColumnEncoder & extraEncoder, const std::string & column)
{
	ScopedColumnEncoderContext scopedContext(encoderContext, extraEncoder);
	return ColumnEncoder::columnEncoder()->encode(column);
}

std::string encodeExtra(const ColumnEncoderContext & encoderContext, ColumnEncoder & extraEncoder, const std::string & value)
{
	ScopedColumnEncoderContext scopedContext(encoderContext, extraEncoder);
	return extraEncoder.encode(value);
}

std::string payloadJson(const ColumnEncoderContext & encoderContext, ColumnEncoder & extraEncoder,
						const std::string & firstColumn,
						const std::string & secondColumn,
						const std::string & extraValue)
{
	const std::string encodedFirstColumn = encodeColumn(encoderContext, extraEncoder, firstColumn);
	const std::string encodedSecondColumn = encodeColumn(encoderContext, extraEncoder, secondColumn);
	const std::string encodedExtraValue = encodeExtra(encoderContext, extraEncoder, extraValue);

	Json::Value payload(Json::objectValue);
	payload[encodedFirstColumn] = encodedExtraValue;
	payload[encodedExtraValue] = encodedFirstColumn;
	payload["nested"][0] = encodedSecondColumn;
	payload["nested"][1]["label"] = std::string("label:") + encodedExtraValue;
	payload["nested"][1]["formula"] = encodedFirstColumn + " + " + encodedSecondColumn;
	return payload.toStyledString();
}

Json::Value decodePayload(const std::string & payload, const std::string & encoderContextJson, ColumnEncoder & extraEncoder)
{
	return decodeColumnJson(payload.c_str(), encoderContextJson.c_str(), extraEncoder);
}

void verifyDecodedPayload(const Json::Value & decoded,
						  const std::string & firstColumn,
						  const std::string & secondColumn,
						  const std::string & extraValue)
{
	QVERIFY2(decoded.isObject(), "Decoded payload should be a JSON object.");
	QVERIFY2(decoded.isMember(firstColumn), "Encoded object member name was not decoded with the expected dataset column context.");
	QVERIFY2(decoded.isMember(extraValue), "Encoded extra-option member name was not decoded with the expected extra-option context.");
	QCOMPARE(QString::fromStdString(decoded[firstColumn].asString()), QString::fromStdString(extraValue));
	QCOMPARE(QString::fromStdString(decoded[extraValue].asString()), QString::fromStdString(firstColumn));
	QCOMPARE(QString::fromStdString(decoded["nested"][0].asString()), QString::fromStdString(secondColumn));
	QCOMPARE(QString::fromStdString(decoded["nested"][1]["label"].asString()), QString::fromStdString("label:" + extraValue));
	QCOMPARE(QString::fromStdString(decoded["nested"][1]["formula"].asString()), QString::fromStdString(firstColumn + " + " + secondColumn));
}

std::string captureError(const std::function<void()> & function)
{
	try
	{
		function();
	}
	catch(const std::exception & exception)
	{
		return exception.what();
	}

	return "";
}
}

void TestColumnEncoderContext::init()
{
	ColumnEncoder::columnEncoder()->setCurrentNames(emptyNames());
}

void TestColumnEncoderContext::cleanup()
{
	ColumnEncoder::columnEncoder()->setCurrentNames(emptyNames());
}

void TestColumnEncoderContext::scopedContextsDecodeInterleavedAndRestoreLiveState()
{
	ColumnEncoder extraEncoder(ExtraOptionsPrefix);

	const ColumnEncoderContext liveContext = context(
		names({{"live group", columnType::nominal}, {"live score", columnType::scale}}),
		names({{"live factor A", columnType::unknown}, {"live factor B", columnType::unknown}})
	);

	activateContext(liveContext, extraEncoder);

	const std::string livePayload = payloadJson(liveContext, extraEncoder, "live group", "live score", "live factor A");

	const ColumnEncoderContext firstArchiveContext = context(
		names({{"archive A group", columnType::nominal}, {"archive A score", columnType::scale}}),
		names({{"archive A level", columnType::unknown}})
	);
	const ColumnEncoderContext secondArchiveContext = context(
		names({{"archive B group", columnType::ordinal}, {"archive B score", columnType::scale}}),
		names({{"archive B level", columnType::unknown}})
	);
	const ColumnEncoderContext thirdArchiveContext = context(
		names({{"archive C group", columnType::nominal}, {"archive C score", columnType::scale}}),
		names({{"archive C level", columnType::unknown}})
	);

	const std::string firstArchivePayload = payloadJson(firstArchiveContext, extraEncoder, "archive A group", "archive A score", "archive A level");
	const std::string secondArchivePayload = payloadJson(secondArchiveContext, extraEncoder, "archive B group", "archive B score", "archive B level");
	const std::string thirdArchivePayload = payloadJson(thirdArchiveContext, extraEncoder, "archive C group", "archive C score", "archive C level");

	verifyDecodedPayload(decodePayload(livePayload, "", extraEncoder), "live group", "live score", "live factor A");

	verifyDecodedPayload(decodePayload(firstArchivePayload, contextJson(firstArchiveContext), extraEncoder), "archive A group", "archive A score", "archive A level");
	verifyDecodedPayload(decodePayload(livePayload, "", extraEncoder), "live group", "live score", "live factor A");

	verifyDecodedPayload(decodePayload(secondArchivePayload, contextJson(secondArchiveContext), extraEncoder), "archive B group", "archive B score", "archive B level");
	verifyDecodedPayload(decodePayload(livePayload, "", extraEncoder), "live group", "live score", "live factor A");

	verifyDecodedPayload(decodePayload(thirdArchivePayload, contextJson(thirdArchiveContext), extraEncoder), "archive C group", "archive C score", "archive C level");
	verifyDecodedPayload(decodePayload(livePayload, "", extraEncoder), "live group", "live score", "live factor A");

	verifyDecodedPayload(decodePayload(firstArchivePayload, contextJson(firstArchiveContext), extraEncoder), "archive A group", "archive A score", "archive A level");
	verifyDecodedPayload(decodePayload(thirdArchivePayload, contextJson(thirdArchiveContext), extraEncoder), "archive C group", "archive C score", "archive C level");
	verifyDecodedPayload(decodePayload(secondArchivePayload, contextJson(secondArchiveContext), extraEncoder), "archive B group", "archive B score", "archive B level");

	verifyDecodedPayload(decodePayload(livePayload, "", extraEncoder), "live group", "live score", "live factor A");
	QVERIFY2(ColumnEncoder::columnEncoder()->currentNames() == liveContext.columns(), "Dataset column context was not restored after scoped decodes.");
	QVERIFY2(extraEncoder.currentNames() == liveContext.extra(), "Extra-option context was not restored after scoped decodes.");
}

void TestColumnEncoderContext::malformedContextDoesNotMutateLiveState()
{
	ColumnEncoder extraEncoder(ExtraOptionsPrefix);

	const ColumnEncoderContext liveContext = context(
		names({{"live group", columnType::nominal}, {"live score", columnType::scale}}),
		names({{"live factor", columnType::unknown}})
	);

	activateContext(liveContext, extraEncoder);

	const std::string livePayload = payloadJson(liveContext, extraEncoder, "live group", "live score", "live factor");

	const std::string parseError = captureError([&]()
	{
		decodePayload(livePayload, "{not-json", extraEncoder);
	});
	const std::string parseErrorMessage = "Unexpected parse error: " + parseError;
	QVERIFY2(parseError.find("Could not parse column encoder context JSON.") != std::string::npos,
			 parseErrorMessage.c_str());
	verifyDecodedPayload(decodePayload(livePayload, "", extraEncoder), "live group", "live score", "live factor");

	const std::string schemaError = captureError([&]()
	{
		decodePayload(livePayload, "{\"version\":1,\"columns\":{},\"extra\":[]}", extraEncoder);
	});
	const std::string schemaErrorMessage = "Unexpected schema error: " + schemaError;
	QVERIFY2(schemaError.find("Column encoder context field 'columns' must be an array.") != std::string::npos,
			 schemaErrorMessage.c_str());
	verifyDecodedPayload(decodePayload(livePayload, "", extraEncoder), "live group", "live score", "live factor");

	QVERIFY2(ColumnEncoder::columnEncoder()->currentNames() == liveContext.columns(), "Dataset column context changed after malformed context errors.");
	QVERIFY2(extraEncoder.currentNames() == liveContext.extra(), "Extra-option context changed after malformed context errors.");
}

void TestColumnEncoderContext::stringLiteralReferencesAreCollectedButNotRewritten()
{
	ColumnEncoder::columnEncoder()->setCurrentNames(names({{"LL", columnType::scale}, {"Operator", columnType::nominal}}));

	const std::string script =
		"mean(data$LL)\n"
		"mean(data[[\"LL\"]])\n"
		"subset(data, select = \"Operator\")\n"
		"paste(\"the LL column\")\n";

	std::set<std::string> columnsFound;
	const std::string encoded = ColumnEncoder::columnEncoder()->encodeRScript(script, &columnsFound);

	QVERIFY2(encoded.find("data$JaspColumn_0_Encoded") != std::string::npos, "Bare column reference was not encoded.");
	QVERIFY2(encoded.find("data[[\"LL\"]]") != std::string::npos, "A whole-string column reference was rewritten, but collection must not rewrite anything.");
	QVERIFY2(encoded.find("select = \"Operator\"") != std::string::npos, "A whole-string column reference was rewritten, but collection must not rewrite anything.");
	QVERIFY2(encoded.find("paste(\"the LL column\")") != std::string::npos, "A string that merely contains a column name was rewritten.");

	QVERIFY2(columnsFound.count("LL") == 1, "The whole-string reference to LL was not collected.");
	QVERIFY2(columnsFound.count("Operator") == 1, "The whole-string reference to Operator was not collected.");
	QVERIFY2(columnsFound.size() == 2, "Unexpected columns were collected: the bare reference and the two whole-string references are the same columns and \"the LL column\" is not a reference.");
}

void TestColumnEncoderContext::stringLiteralCollectionIsCommentEscapeAndRawStringAware()
{
	ColumnEncoder::columnEncoder()->setCurrentNames(names({{"LL", columnType::scale}, {"Operator", columnType::nominal}}));

	//There are no bare column references in this script, so collect-only means the script must come back unchanged
	const std::string script =
		"# a comment mentioning \"LL\" and 'Operator' should not desync the scanner\n"
		"note <- 'it\\'s fine'\n"
		"ref  <- data[[\"Operator\"]]\n"
		"raw  <- r\"(LL)\"\n";

	std::set<std::string> columnsFound;
	const std::string encoded = ColumnEncoder::columnEncoder()->encodeRScript(script, &columnsFound);

	QVERIFY2(encoded == script, "The script was rewritten even though it contains no bare column references.");

	QVERIFY2(columnsFound.count("Operator") == 1, "The quoted reference after the comment and the escaped quote was missed.");
	QVERIFY2(columnsFound.count("LL") == 0, "A raw string that is a column name was collected, but raw strings keep meaning data.");

	//An unterminated string stops the scan: nothing after it can be trusted
	const std::string brokenScript = "good <- data[[\"LL\"]]\nbroken <- 'unterminated\nmore <- data[[\"Operator\"]]\n";

	std::set<std::string> brokenFound;
	ColumnEncoder::columnEncoder()->encodeRScript(brokenScript, &brokenFound);

	QVERIFY2(brokenFound.count("LL") == 1, "References before the unterminated string should still be collected.");
	QVERIFY2(brokenFound.count("Operator") == 0, "Nothing after an unterminated string can be trusted, so it is not collected.");
}

void TestColumnEncoderContext::prefixedEncodingCollectsStringLiteralsWithoutRewriting()
{
	ColumnEncoder::columnEncoder()->setCurrentNames(names({{"LL", columnType::scale}}));

	std::map<std::string, std::set<std::string>> prefixedColumnsFound;
	const std::string encoded = ColumnEncoder::columnEncoder()->encodeRScript("value <- data.[[\"LL\"]]", prefixedColumnsFound, {"data."});

	QVERIFY2(encoded.find("data.[[\"LL\"]]") != std::string::npos, "The whole-string reference was rewritten in prefixed encoding.");
	QVERIFY2(prefixedColumnsFound[""].count("LL") == 1, "The unprefixed run did not collect the whole-string reference.");
}

void TestColumnEncoderContext::commentAndEscapeDesyncIsFixed()
{
	ColumnEncoder::columnEncoder()->setCurrentNames(names({{"LL", columnType::scale}}));

	//An apostrophe in a comment used to put the scanner in string mode and suppress every match after it
	const std::string script = "# don't use data$LL here\nmean(data$LL)\n";

	std::set<std::string> columnsFound;
	const std::string encoded = ColumnEncoder::columnEncoder()->encodeRScript(script, &columnsFound);

	QVERIFY2(encoded.find("mean(data$JaspColumn_0_Encoded)") != std::string::npos, "The bare reference after the comment was suppressed by the apostrophe in the comment.");
	QVERIFY2(encoded.find("# don't use data$LL here") != std::string::npos, "The comment was not left alone.");
	QVERIFY2(columnsFound.count("LL") == 1, "LL was not collected.");

	//An escaped quote inside a string used to close it early and reopen string mode at the real end
	const std::string escapeScript = "note <- 'it\\'s fine'\nmean(data$LL)\n";

	std::set<std::string> escapeFound;
	const std::string escapeEncoded = ColumnEncoder::columnEncoder()->encodeRScript(escapeScript, &escapeFound);

	QVERIFY2(escapeEncoded.find("mean(data$JaspColumn_0_Encoded)") != std::string::npos, "The bare reference after the escaped quote was suppressed.");
}

void TestColumnEncoderContext::newlinesAndTabsInsideStringsStayStringContent()
{
	ColumnEncoder::columnEncoder()->setCurrentNames(names({{"LL", columnType::scale}}));

	//R string literals may contain raw newlines and tabs: only the unescaped delimiter ends
	//the string, so the scanner must sail over both and still trust the code after the string.
	const std::string rawScript =
		"text <- \"line1\nline2\tend\"\n"
		"mean(data$LL)\n";

	std::set<std::string> rawFound;
	const std::string rawEncoded = ColumnEncoder::columnEncoder()->encodeRScript(rawScript, &rawFound);

	QVERIFY2(rawFound.count("LL") == 1, "The raw newline and tab inside the string desynced the scan.");
	QVERIFY2(rawEncoded.find("\"line1\nline2\tend\"") != std::string::npos, "The string content was rewritten even though it is not a column reference.");
	QVERIFY2(rawEncoded.find("mean(data$JaspColumn_0_Encoded)") != std::string::npos, "The reference after the multi-line string was not rewritten.");

	//The two-character escape sequences \n and \t must not end the string either
	const std::string escapeScript = "note <- \"a\\nb\\tc\"\nmean(data$LL)\n";

	std::set<std::string> escapeFound;
	const std::string escapeEncoded = ColumnEncoder::columnEncoder()->encodeRScript(escapeScript, &escapeFound);

	QVERIFY2(escapeFound.count("LL") == 1, "The \\n and \\t escape sequences desynced the scan.");
	QVERIFY2(escapeEncoded.find("mean(data$JaspColumn_0_Encoded)") != std::string::npos, "The reference after the escape sequences was not rewritten.");

	//A lone backslash at the very end of the text must not walk the scanner out of bounds
	const std::string eofBackslash = "mean(data$LL)\nnot <- \"dangling\\";

	std::set<std::string> eofFound;
	const std::string eofEncoded = ColumnEncoder::columnEncoder()->encodeRScript(eofBackslash, &eofFound);

	QVERIFY2(eofFound.count("LL") == 1, "The dangling escape corrupted the scan before it.");
}

void TestColumnEncoderContext::pathologicalNamesRawStringsAndBackticksAreHandled()
{
	//An apostrophe inside a backticked symbol is symbol content, not a string start: both references
	//must be rewritten (the second used to be suppressed by the desync the first one caused)
	ColumnEncoder::columnEncoder()->setCurrentNames(names({{"don't", columnType::scale}}));

	const std::string apostropheScript = "x <- data$`don't` + data$`don't`\n";

	std::set<std::string> apostropheFound;
	const std::string apostropheEncoded = ColumnEncoder::columnEncoder()->encodeRScript(apostropheScript, &apostropheFound);

	QVERIFY2(apostropheEncoded.find("data$`JaspColumn_0_Encoded` + data$`JaspColumn_0_Encoded`") != std::string::npos, "The second backticked reference with an apostrophe was suppressed by the desync.");
	QVERIFY2(apostropheFound.count("don't") == 1, "The backticked reference with an apostrophe was not collected.");

	//A columnname starting with a quote can only be referenced through backticks: a genuine string
	//"x" must never be rewritten (the scanner used to match the name at the string start and corrupt it)
	ColumnEncoder::columnEncoder()->setCurrentNames(names({{"\"x", columnType::scale}}));

	const std::string quoteNameScript = "y <- \"x\"\nz <- data$`\"x`\n";

	std::set<std::string> quoteNameFound;
	const std::string quoteNameEncoded = ColumnEncoder::columnEncoder()->encodeRScript(quoteNameScript, &quoteNameFound);

	QVERIFY2(quoteNameEncoded.find("y <- \"x\"") != std::string::npos, "The string was corrupted by a match of the quote-starting columnname at the string start.");
	QVERIFY2(quoteNameEncoded.find("z <- data$`JaspColumn_0_Encoded`") != std::string::npos, "The backticked reference to the quote-starting columnname was not rewritten.");
	QVERIFY2(quoteNameFound.count("\"x") == 1, "The backticked reference to the quote-starting columnname was not collected.");

	//A raw string is data: its content is never rewritten or collected, and the code after a raw
	//string whose content contains quotes must still be rewritten
	ColumnEncoder::columnEncoder()->setCurrentNames(names({{"LL", columnType::scale}}));

	const std::string rawStringScript = "x <- r\"---(say \"LL\" out loud)---\"\n";

	std::set<std::string> rawStringFound;
	const std::string rawStringEncoded = ColumnEncoder::columnEncoder()->encodeRScript(rawStringScript, &rawStringFound);

	QVERIFY2(rawStringEncoded == rawStringScript, "The raw string content was rewritten or the scan desynced on its embedded quotes.");
	QVERIFY2(rawStringFound.count("LL") == 0, "The raw string content was collected even though raw strings keep meaning data.");

	const std::string rawStringThenCodeScript = "x <- r\"---(say \"LL\" out loud)---\"\nmean(data$LL)\n";

	std::set<std::string> rawStringThenCodeFound;
	const std::string rawStringThenCodeEncoded = ColumnEncoder::columnEncoder()->encodeRScript(rawStringThenCodeScript, &rawStringThenCodeFound);

	QVERIFY2(rawStringThenCodeEncoded.find("mean(data$JaspColumn_0_Encoded)") != std::string::npos, "The reference after the raw string with embedded quotes was suppressed.");
	QVERIFY2(rawStringThenCodeFound.count("LL") == 1, "The reference after the raw string was not collected.");

	//A backticked symbol is one name: a columnname inside it is not a reference (this used to corrupt
	//`data$LL` into `data$<encoded>`), while a symbol that is exactly the columnname still matches
	ColumnEncoder::columnEncoder()->setCurrentNames(names({{"LL", columnType::scale}}));

	const std::string backtickScript = "x <- `data$LL`\nmean(data$`LL`)\n";

	std::set<std::string> backtickFound;
	const std::string backtickEncoded = ColumnEncoder::columnEncoder()->encodeRScript(backtickScript, &backtickFound);

	QVERIFY2(backtickEncoded.find("`data$LL`") != std::string::npos, "The backticked symbol data$LL was corrupted by a substring match.");
	QVERIFY2(backtickEncoded.find("mean(data$`JaspColumn_0_Encoded`)") != std::string::npos, "The backticked symbol that is exactly the columnname was not rewritten.");
	QVERIFY2(backtickFound.count("LL") == 1, "The backticked symbol that is exactly the columnname was not collected.");

	//An unterminated raw string stops the scan: nothing after it can be trusted
	const std::string unterminatedScript = "x <- r\"(LL\nmean(data$LL)\n";

	std::set<std::string> unterminatedFound;
	ColumnEncoder::columnEncoder()->encodeRScript(unterminatedScript, &unterminatedFound);

	QVERIFY2(unterminatedFound.count("LL") == 0, "Something after an unterminated raw string was collected anyway.");
}

void TestColumnEncoderContext::colsPlusTypesForResolvesColumnsAndTypes()
{
	ColumnEncoder::columnEncoder()->setCurrentNames(names({{"LL", columnType::scale}, {"part length", columnType::ordinal}}));

	const ColumnEncoder::colsPlusTypes requested = ColumnEncoder::columnEncoder()->colsPlusTypesFor(
		{"LL", "LL.nominal", "part length.ordinal", "notAColumn", "LL.notAType"});

	QVERIFY2(requested.count({"LL",						columnType::scale})		== 1, "An exact columnname did not get its current type.");
	QVERIFY2(requested.count({"LL.nominal",				columnType::nominal})	== 1, "A typed reference did not keep its name and get the asked type.");
	QVERIFY2(requested.count({"part length.ordinal",	columnType::ordinal})	== 1, "A typed reference to a non-syntactic columnname was not resolved.");
	QVERIFY2(requested.size() == 3, "colsPlusTypesFor resolved something that is not a column (reference).");
}

QTEST_MAIN(TestColumnEncoderContext)
