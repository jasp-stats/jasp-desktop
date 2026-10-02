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

// The two datasets carry the same raw column name ("Score"); DataSet::setupEncoderPrefix() makes
// their encoded names differ by embedding the dataset id, the distinct prefixes simulate that.
void makeTwoDataSets(ColumnEncoder & one, ColumnEncoder & two)
{
	one.setCurrentNames(names({{"Score", columnType::scale}, {"Group", columnType::nominal}}));
	two.setCurrentNames(names({{"Score", columnType::scale}, {"Response", columnType::nominal}}));
}

Json::Value twoDataSetOptions()
{
	Json::Value options(Json::objectValue);

	options["dependent"]	= Json::Value(Json::objectValue);
	options["dependent"]["value"]	= Json::Value(Json::arrayValue);
	options["dependent"]["value"].append("Score");
	options["dependent"]["types"]	= Json::Value(Json::arrayValue);
	options["dependent"]["types"].append("scale");

	options["covariate"]	= Json::Value(Json::objectValue);
	options["covariate"]["value"]	= Json::Value(Json::arrayValue);
	options["covariate"]["value"].append("Score");
	options["covariate"]["types"]	= Json::Value(Json::arrayValue);
	options["covariate"]["types"].append("scale");

	options[".meta"]				= Json::Value(Json::objectValue);
	options[".meta"]["dependent"]	= Json::Value(Json::objectValue);
	options[".meta"]["dependent"]["shouldEncode"]	= true;
	options[".meta"]["dependent"]["dataSetId"]		= 1;
	options[".meta"]["dependent"]["filterId"]		= 11;
	options[".meta"]["covariate"]	= Json::Value(Json::objectValue);
	options[".meta"]["covariate"]["shouldEncode"]	= true;
	options[".meta"]["covariate"]["dataSetId"]		= 2;
	options[".meta"]["covariate"]["filterId"]		= 22;

	return options;
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

void TestColumnEncoderContext::encodePerDataSetRoutesOptionsToEncoderOfTheirDataSet()
{
	ColumnEncoder dsOne("JASPColumn_1_"), dsTwo("JASPColumn_2_");
	makeTwoDataSets(dsOne, dsTwo);

	Json::Value options = twoDataSetOptions();

	ColumnEncoder::perDataSetColsPlusTypes colsPerDataSet = ColumnEncoder::encodeColumnNamesinOptionsPerDataSet(
			options, true,
			[&dsOne, &dsTwo](int dataSetId) -> ColumnEncoder *
			{
				if(dataSetId == 1)	return &dsOne;
				if(dataSetId == 2)	return &dsTwo;
				return nullptr;
			},
			1);

	//Both options referred to a column called "Score", but each got the encoded name of its own dataset:
	const std::string encodedOne = dsOne.encode("Score.scale");
	const std::string encodedTwo = dsTwo.encode("Score.scale");

	QCOMPARE(QString::fromStdString(options["dependent"][0].asString()),  QString::fromStdString(encodedOne));
	QCOMPARE(QString::fromStdString(options["covariate"][0].asString()),  QString::fromStdString(encodedTwo));
	QVERIFY2(encodedOne != encodedTwo, "The same raw name in two datasets must not encode to the same name.");

	//And the requested cols+types are attributed per dataset:
	QCOMPARE(colsPerDataSet.size(), size_t(2));
	QVERIFY(colsPerDataSet.count(1) == 1 && colsPerDataSet.count(2) == 1);
	QCOMPARE(colsPerDataSet[1].size(), size_t(1));
	QCOMPARE(colsPerDataSet[2].size(), size_t(1));
	QVERIFY(colsPerDataSet[1].count(std::make_pair(std::string("Score.scale"), columnType::scale)) == 1);
	QVERIFY(colsPerDataSet[2].count(std::make_pair(std::string("Score.scale"), columnType::scale)) == 1);
}

void TestColumnEncoderContext::encodePerDataSetAttributedToPrimaryWithoutProvenance()
{
	ColumnEncoder dsOne("JASPColumn_1_"), dsTwo("JASPColumn_2_");
	makeTwoDataSets(dsOne, dsTwo);

	Json::Value options = twoDataSetOptions();
	options[".meta"]["covariate"].removeMember("dataSetId"); //provenance lost: it must fall back to the primary dataset

	ColumnEncoder::perDataSetColsPlusTypes colsPerDataSet = ColumnEncoder::encodeColumnNamesinOptionsPerDataSet(
			options, true,
			[&dsOne, &dsTwo](int dataSetId) -> ColumnEncoder *
			{
				if(dataSetId == 1)	return &dsOne;
				if(dataSetId == 2)	return &dsTwo;
				return nullptr;
			},
			1);

	QCOMPARE(colsPerDataSet.size(), size_t(1));
	QVERIFY(colsPerDataSet.count(1) == 1);
	QCOMPARE(colsPerDataSet[1].size(), size_t(1)); //dependent and covariate ask for the same Score.scale, and the set deduplicates

	//Without a dataSetId the option is encoded against the primary dataset's encoder as well:
	QCOMPARE(QString::fromStdString(options["covariate"][0].asString()), QString::fromStdString(dsOne.encode("Score.scale")));
}

void TestColumnEncoderContext::encodePerDataSetWithoutResolverIsLegacyEquivalent()
{
	ColumnEncoder * current = ColumnEncoder::columnEncoder();
	current->setCurrentNames(names({{"Score", columnType::scale}, {"Group", columnType::nominal}}));

	Json::Value legacyOptions	= twoDataSetOptions(),
				newOptions		= twoDataSetOptions();

	//Drop provenance: legacy JASP files have no dataSetId in their meta at all.
	legacyOptions[".meta"]["dependent"].removeMember("dataSetId");
	legacyOptions[".meta"]["dependent"].removeMember("filterId");
	legacyOptions[".meta"]["covariate"].removeMember("dataSetId");
	legacyOptions[".meta"]["covariate"].removeMember("filterId");
	newOptions[".meta"]["dependent"].removeMember("dataSetId");
	newOptions[".meta"]["dependent"].removeMember("filterId");
	newOptions[".meta"]["covariate"].removeMember("dataSetId");
	newOptions[".meta"]["covariate"].removeMember("filterId");

	ColumnEncoder::colsPlusTypes legacyCols = ColumnEncoder::encodeColumnNamesinOptions(legacyOptions, true);

	ColumnEncoder::perDataSetColsPlusTypes newColsPerDataSet = ColumnEncoder::encodeColumnNamesinOptionsPerDataSet(newOptions, true, nullptr, 0);

	QCOMPARE(newColsPerDataSet.size(), size_t(1)); //everything attributed to the single (primary) dataset
	QVERIFY(newColsPerDataSet.count(0) == 1);

	ColumnEncoder::colsPlusTypes newCols = newColsPerDataSet[0];

	QVERIFY2(newCols == legacyCols, "Per-dataset encoding without resolver must collect the very same cols+types as the legacy function.");
	QCOMPARE(QString::fromStdString(newOptions.toStyledString()), QString::fromStdString(legacyOptions.toStyledString()));

	//And the merged-map variant of the new function must match the legacy output too:
	Json::Value mergedOptions = twoDataSetOptions();
	mergedOptions[".meta"]["dependent"].removeMember("dataSetId");
	mergedOptions[".meta"]["dependent"].removeMember("filterId");
	mergedOptions[".meta"]["covariate"].removeMember("dataSetId");
	mergedOptions[".meta"]["covariate"].removeMember("filterId");

	ColumnEncoder::perDataSetColsPlusTypes stillLegacy = ColumnEncoder::encodeColumnNamesinOptionsPerDataSet(mergedOptions, true, nullptr, 0);
	QCOMPARE(QString::fromStdString(mergedOptions.toStyledString()), QString::fromStdString(legacyOptions.toStyledString()));
	(void) stillLegacy;
}

void TestColumnEncoderContext::collectDataSetIdsFromMetaGathersPairsAndUpgrades()
{
	Json::Value meta(Json::objectValue);

	meta["dependent"]["shouldEncode"]	= true;
	meta["dependent"]["dataSetId"]		= 3;
	meta["dependent"]["filterId"]		= 31;
	meta["fixedFactors"]["shouldEncode"]	= true;
	meta["fixedFactors"]["dataSetId"]	= 4;                       //no filterId yet
	meta["nested"][0]["dataSetId"]		= 4;                       //duplicate, no filter
	meta["nested"][1]["dataSetId"]		= 4;
	meta["nested"][1]["filterId"]		= 42;                      //upgrades the -1 of dataset 4
	meta["nested"][2]["dataSetId"]		= 5;
	meta["nested"][2]["filterId"]		= 51;
	meta["nested"][2]["filterId"]		= 51;
	meta["bogus"]["dataSetId"]			= "seven";                 //not an int: ignored
	meta["someString"]					= "hello";

	std::map<int, int> dataSetFilterIds;
	ColumnEncoder::collectDataSetIdsFromMeta(meta, dataSetFilterIds);

	QCOMPARE(dataSetFilterIds.size(), size_t(3));
	QCOMPARE(dataSetFilterIds[3], 31);
	QCOMPARE(dataSetFilterIds[4], 42); //first seen without filter (-1), upgraded by the later occurrence
	QCOMPARE(dataSetFilterIds[5], 51);
}

QTEST_MAIN(TestColumnEncoderContext)
