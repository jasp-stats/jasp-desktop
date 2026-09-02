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

void TestColumnEncoderContext::currentEncoderReTargetsOnSwitchAndClearsOnDestruction()
{
	ColumnEncoder * defaultEncoder = ColumnEncoder::columnEncoder();
	QVERIFY(defaultEncoder != nullptr);

	ColumnEncoder first("firstGroup_");
	ColumnEncoder second("secondGroup_");

	first .setCurrentNames(names({{"first score",	columnType::scale}}));
	second.setCurrentNames(names({{"second score",	columnType::scale}}));

	//The indirection must follow whichever encoder is marked "current" (the multi-dataset path
	//points it at the shown dataset's encoder on setShownDataSet / provideAndUpdateDataSet).
	ColumnEncoder::setCurrentEncoder(&first);
	QCOMPARE(ColumnEncoder::currentEncoder(), &first);
	QVERIFY2(ColumnEncoder::isColumnName("first score"),		"Current encoder (first) should know 'first score'.");
	QVERIFY2(!ColumnEncoder::isColumnName("second score"),	"Non-current encoder's column must not be seen.");

	ColumnEncoder::setCurrentEncoder(&second);
	QCOMPARE(ColumnEncoder::currentEncoder(), &second);
	QVERIFY2(ColumnEncoder::isColumnName("second score"),	"Current encoder (second) should know 'second score'.");
	QVERIFY2(!ColumnEncoder::isColumnName("first score"),	"Switching away must drop the first encoder's columns.");

	//Destroying the encoder that is currently pointed-at must clear the indirection so a later use
	//cannot dereference a dangling encoder (the ~ColumnEncoder guard for the per-dataset case).
	auto * transient = new ColumnEncoder("transient_");
	transient->setCurrentNames(names({{"transient score", columnType::scale}}));
	ColumnEncoder::setCurrentEncoder(transient);
	QCOMPARE(ColumnEncoder::currentEncoder(), transient);
	delete transient;
	QCOMPARE(ColumnEncoder::currentEncoder(), nullptr);

	//Next use falls back to the default instance rather than the destroyed one.
	ColumnEncoder * fallback = ColumnEncoder::columnEncoder();
	QVERIFY(fallback != nullptr);
	QVERIFY2(fallback != transient, "Fallback must not return the destroyed encoder.");
	QCOMPARE(fallback, defaultEncoder);

	//Leave the global pointing at the default so init/cleanup expectations hold for later tests.
	ColumnEncoder::setCurrentEncoder(defaultEncoder);
}

void TestColumnEncoderContext::rCommanderEncodingKeepsArgumentNamesIntact()
{
	//Regression for the built-in R Console: short column names such as "cl" and "g" used to be
	//substituted *inside* function/argument names by the naive encodeAll path, turning
	//	lavaan::sem("y~x", data=data, cluster="cl", missing="ml.x", group="g")
	//into an unparseable script with mangled arguments ('...uster', 'missin...', '...roup').
	//encodeRScriptCommander must honour word boundaries while still encoding column names that
	//appear inside string literals (lavaan formulas / quoted arguments).
	ColumnEncoder * encoder = ColumnEncoder::columnEncoder();

	encoder->setCurrentNames(names({
		{"y",  columnType::unknown},
		{"x",  columnType::unknown},
		{"cl", columnType::unknown},
		{"g",  columnType::unknown}
	}));

	const std::string encY  = encoder->encode("y");
	const std::string encX  = encoder->encode("x");
	const std::string encCl = encoder->encode("cl");
	const std::string encG  = encoder->encode("g");

	const std::string userScript =
		"lavaan::sem(\"y~x\", data=data, cluster=\"cl\", missing=\"ml.x\", group=\"g\")";

	const std::string encoded = encoder->encodeRScriptCommander(userScript);

	//Column references (also inside the quoted formula and quoted argument values) are encoded...
	const std::string expected =
		"lavaan::sem(\"" + encY + "~" + encX + "\", data=data, cluster=\"" + encCl +
		"\", missing=\"ml.x\", group=\"" + encG + "\")";
	QCOMPARE(QString::fromStdString(encoded), QString::fromStdString(expected));

	//...while the argument names themselves stay intact.
	QVERIFY2(encoded.find("cluster=\"") != std::string::npos, "Argument name 'cluster' was mangled by the commander encoder.");
	QVERIFY2(encoded.find("missing=\"") != std::string::npos, "Argument name 'missing' was mangled by the commander encoder.");
	QVERIFY2(encoded.find("group=\"")   != std::string::npos, "Argument name 'group' was mangled by the commander encoder.");

	//The lavaan option value ml.x must be preserved: the '.' keeps the trailing 'x' bound, so it is not a column ref.
	QVERIFY2(encoded.find("\"ml.x\"") != std::string::npos, "lavaan option value 'ml.x' should not be encoded.");

	//Decoding the script (as the engine does to the result/error via decodeAll) restores the original text.
	QCOMPARE(QString::fromStdString(encoder->decodeAll(encoded)), QString::fromStdString(userScript));
}

void TestColumnEncoderContext::encodeRScriptSkipsStringLiteralsAndSubstringsByDefault()
{
	//The plain public overload (encodeInsideStrings defaults to false) must honour word boundaries AND leave
	//string literals untouched: a short column ("cl") may not eat into an argument/identifier ("cluster"), and
	//column names quoted inside a string ("cl and g") stay verbatim. This is the behaviour every analysis form,
	//filter and JAGS box relies on, and it is the raw-map (non-merged) path of the refactored worker.
	ColumnEncoder * encoder = ColumnEncoder::columnEncoder();

	encoder->setCurrentNames(names({
		{"y",  columnType::unknown},
		{"x",  columnType::unknown},
		{"cl", columnType::unknown},
		{"g",  columnType::unknown}
	}));

	const std::string encY = encoder->encode("y");
	const std::string encX = encoder->encode("x");

	const std::string userScript = "y ~ x + \"cl and g\" + cluster";

	std::set<std::string> columnNamesFound;
	const std::string encoded = encoder->encodeRScript(userScript, &columnNamesFound);

	const std::string expected = encY + " ~ " + encX + " + \"cl and g\" + cluster";
	QCOMPARE(QString::fromStdString(encoded), QString::fromStdString(expected));

	//Only the free, out-of-string references were encoded.
	QVERIFY2(columnNamesFound == (std::set<std::string>{"x", "y"}), "Only the free column references outside string literals should be reported.");

	//The string literal survived untouched and the identifier 'cluster' was not clobbered by the column "cl".
	QVERIFY2(encoded.find("\"cl and g\"") != std::string::npos, "Column names inside a string literal must not be encoded by the default overload.");
	QVERIFY2(encoded.find("cluster")     != std::string::npos, "Identifier 'cluster' must not be clobbered by the short column 'cl'.");
}

void TestColumnEncoderContext::encodeRScriptEncodesInsideStringLiteralsWhenRequested()
{
	//Opting in with encodeInsideStrings=true must additionally rewrite column references that appear inside
	//string literals (e.g. a formula assigned to a variable) while still respecting word boundaries.
	ColumnEncoder * encoder = ColumnEncoder::columnEncoder();

	encoder->setCurrentNames(names({
		{"y", columnType::unknown},
		{"x", columnType::unknown}
	}));

	const std::string encY = encoder->encode("y");
	const std::string encX = encoder->encode("x");

	const std::string userScript = "formula <- \"y ~ x\"";

	std::set<std::string> columnNamesFound;
	const std::string encoded = encoder->encodeRScript(userScript, &columnNamesFound, /*encodeInsideStrings=*/true);

	const std::string expected = "formula <- \"" + encY + " ~ " + encX + "\"";
	QCOMPARE(QString::fromStdString(encoded), QString::fromStdString(expected));
	QVERIFY2(columnNamesFound == (std::set<std::string>{"x", "y"}), "Both column names inside the string literal should be reported when encodeInsideStrings is true.");
}

void TestColumnEncoderContext::encodeRScriptPrefixedTracksColumnsPerPrefix()
{
	//The prefix-aware overload reports, per allowed prefix (plus the implicit empty prefix), which columns it
	//encoded. A bare "y" is caught by the empty prefix; a "data.y" reference only matches under the "data." prefix.
	ColumnEncoder * encoder = ColumnEncoder::columnEncoder();

	encoder->setCurrentNames(names({ {"y", columnType::unknown} }));

	const std::string encY = encoder->encode("y");

	const std::string userScript = "y + data.y";

	std::map<std::string, std::set<std::string>> prefixedColumnsFound;
	const std::string encoded = encoder->encodeRScript(userScript, prefixedColumnsFound, std::set<std::string>{"data."});

	const std::string expected = encY + " + data." + encY;
	QCOMPARE(QString::fromStdString(encoded), QString::fromStdString(expected));

	QVERIFY2(prefixedColumnsFound[""]      == (std::set<std::string>{"y"}), "The bare column reference should be reported under the empty prefix.");
	QVERIFY2(prefixedColumnsFound["data."] == (std::set<std::string>{"y"}), "The prefixed column reference should be reported under the 'data.' prefix.");
}

void TestColumnEncoderContext::encodeRScriptMergesExtraEncodersOnlyForCommander()
{
	//Regression for the refactored worker's useMergedMaps flag: the plain overload must encode against ONLY this
	//encoder's own names, whereas encodeRScriptCommander merges the extra encoders (matching decodeAll on the
	//result). An extra encoder's column must therefore be left verbatim by encodeRScript but encoded by the commander.
	ColumnEncoder * encoder = ColumnEncoder::columnEncoder();
	encoder->setCurrentNames(names({ {"y", columnType::unknown} }));

	ColumnEncoder extraEncoder(ExtraOptionsPrefix);
	extraEncoder.setCurrentNames(names({ {"extraCol", columnType::unknown} }));

	const std::string encY        = encoder->encode("y");
	const std::string encExtraCol = extraEncoder.encode("extraCol");

	const std::string userScript = "y + extraCol";

	//Raw path: only "y" (this encoder's own column) is encoded; the extra encoder's "extraCol" is untouched.
	std::set<std::string> columnNamesFound;
	const std::string rawEncoded = encoder->encodeRScript(userScript, &columnNamesFound);
	QCOMPARE(QString::fromStdString(rawEncoded), QString::fromStdString(encY + " + extraCol"));
	QVERIFY2(columnNamesFound == (std::set<std::string>{"y"}), "The plain overload must not see the extra encoder's columns.");

	//Merged path: the commander also encodes the extra encoder's "extraCol".
	const std::string commanderEncoded = encoder->encodeRScriptCommander(userScript);
	QCOMPARE(QString::fromStdString(commanderEncoded), QString::fromStdString(encY + " + " + encExtraCol));
}

QTEST_MAIN(TestColumnEncoderContext)
