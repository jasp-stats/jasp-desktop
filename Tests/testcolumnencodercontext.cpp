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

// v2 replay is decode-only (the captured namespace lands in the global table for the scope's
// duration), so payload tokens are minted as pure strings - exactly the C1 shape the replay
// block writes - instead of the v1 "swap the live encoder, then encode" dance.
std::string encodeColumn(const ColumnEncoderContext & encoderContext, ColumnEncoder & /*extraEncoder*/, const std::string & column)
{
	const std::string prefix = encoderContext.prefix().empty() ? ColumnEncoder::columnEncoder()->encodePrefix() : encoderContext.prefix();
	const std::string token	 = ColumnEncoder::tokenForNameInNames(encoderContext.columns(), column, prefix);

	Q_ASSERT_X(!token.empty(), "encodeColumn", "test context must contain the column it encodes");
	return token;
}

std::string encodeExtra(const ColumnEncoderContext & encoderContext, ColumnEncoder & extraEncoder, const std::string & value)
{
	const std::string token = ColumnEncoder::tokenForNameInNames(encoderContext.extra(), value, extraEncoder.encodePrefix());

	Q_ASSERT_X(!token.empty(), "encodeExtra", "test context must contain the extra value it encodes");
	return token;
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
	ColumnEncoder::resetForTests();		//forget every block left behind by the Phase-1 view tests
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

	// v2: an unsupported version is rejected ... before anything touches the table.
	const std::string versionError = captureError([&]()
	{
		decodePayload(livePayload, "{\"version\":3,\"columns\":[],\"extra\":[]}", extraEncoder);
	});
	const std::string versionErrorMessage = "Unexpected version error: " + versionError;
	QVERIFY2(versionError.find("Unsupported column encoder context version.") != std::string::npos,
			 versionErrorMessage.c_str());

	// ... and a syntactically fine context with malformed entries likewise (nothing half-written).
	const std::string entryError = captureError([&]()
	{
		decodePayload(livePayload, "{\"version\":2,\"columns\":[{\"name\":42,\"type\":\"scale\"}],\"extra\":[]}", extraEncoder);
	});
	const std::string entryErrorMessage = "Unexpected entry error: " + entryError;
	QVERIFY2(entryError.find("must contain objects with string 'name' and 'type' fields.") != std::string::npos,
			 entryErrorMessage.c_str());

	QVERIFY2(ColumnEncoder::columnEncoder()->currentNames() == liveContext.columns(), "Dataset column context changed after malformed context errors.");
	QVERIFY2(extraEncoder.currentNames() == liveContext.extra(), "Extra-option context changed after malformed context errors.");

	// v2 pin: the live TABLE entries survived every malformed replay untouched (v1 corrupted
	// nothing either, but only because it restored by reminting - v2 never writes on a throw).
	QCOMPARE(ColumnEncoder::decode(encodeColumn(liveContext, extraEncoder, "live score")), std::string("live score"));
	QCOMPARE(ColumnEncoder::decode(encodeExtra(liveContext, extraEncoder, "live factor")), std::string("live factor"));
}

void TestColumnEncoderContext::replayNeverMutatesLiveEncodersAndNestsExactly()
{
	//The v2 contract (mds-plan D1.7): replaying a captured context registers a scoped block of
	//the global decode table and undoes it on exit - the live canonical mint, its filter views
	//and the extras encoder are NEVER mutated (v1 swapped the current encoder's names, which
	//after Phase 1 funnels straight into a live dataset's mint). Nesting restores exactly, and
	//replays that collide with a live namespace (extras, or the same archive slot twice) put the
	//displaced entries back rather than erasing them.
	ColumnEncoder * canonical = ColumnEncoder::createCanonical();
	const ColumnEncoder::colTypeMap liveColumns = names({{"Group", columnType::nominal}, {"Score", columnType::scale}});
	canonical->setCurrentNames(liveColumns);

	ColumnEncoder * liveView = ColumnEncoder::createView(canonical, "JASPColumn_D1_F11_");
	const std::string liveToken = liveView->encode("Score");

	ColumnEncoder extras(ExtraOptionsPrefix);
	extras.setCurrentNames(names({{"live level", columnType::unknown}}));
	const std::string liveExtraToken = extras.encode("live level");			//JaspExtraOptions_0_Encoded

	//An archived namespace: its own host-filter prefix, different columns, and an extras snapshot
	//that deliberately collides with the live extras block on index 0.
	const ColumnEncoderContext archived(
		names({{"Old Score", columnType::scale}}),
		names({{"archived level", columnType::unknown}}),
		"JASPColumn_D2_F21_");

	const std::string archivedToken		= ColumnEncoder::tokenForNameInNames(archived.columns(),	"Old Score",		archived.prefix());
	const std::string archivedExtra		= ColumnEncoder::tokenForNameInNames(archived.extra(),		"archived level",	ExtraOptionsPrefix);

	QCOMPARE(liveExtraToken, archivedExtra);	//same token string, one shared extras namespace (C6)
	QCOMPARE(ColumnEncoder::decode(liveToken), std::string("Score"));
	QCOMPARE(ColumnEncoder::decode(archivedToken), archivedToken);				//archive not live: raw
	QCOMPARE(ColumnEncoder::decode(archivedExtra), std::string("live level"));	//before the replay, live extras answers

	{
		ScopedColumnEncoderContext scope(archived, extras);

		QCOMPARE(ColumnEncoder::decode(archivedToken), std::string("Old Score"));
		QCOMPARE(ColumnEncoder::decode(archivedExtra), std::string("archived level"));	//replay displaces the colliding entry
		QCOMPARE(ColumnEncoder::decode(liveToken), std::string("Score"));				//other live namespaces keep decoding (superset, C4)

		//The whole point vs v1: entering the scope mutated nothing live:
		QVERIFY2(canonical->currentNames() == liveColumns,	"replay must not touch the live canonical mint");
		QCOMPARE(liveView->encode("Score"), liveToken);		//the live view still mints the very same token
		QCOMPARE(extras.currentNames().size(), size_t(1));	//live extras untouched

		//Nesting over the same namespace: another archive of the same slot (colliding prefix AND
		//colliding index 0) must layer over the outer replay and undo back to it, not to nothing:
		{
			const ColumnEncoderContext archived2(names({{"Newer Score", columnType::scale}}), ColumnEncoder::colTypeMap(), "JASPColumn_D2_F21_");
			const std::string archived2Token = ColumnEncoder::tokenForNameInNames(archived2.columns(), "Newer Score", archived2.prefix());

			QCOMPARE(archived2Token, archivedToken);	//same slot, same index: a pure overwrite test

			ScopedColumnEncoderContext inner(archived2, extras);

			QCOMPARE(ColumnEncoder::decode(archivedToken), std::string("Newer Score"));	//inner wins
			QCOMPARE(ColumnEncoder::decode(liveExtraToken), std::string("archived level"));	//inner extra is empty: outer still in effect
		}

		QCOMPARE(ColumnEncoder::decode(archivedToken), std::string("Old Score"));	//outer restored by the undo log
		QCOMPARE(ColumnEncoder::decode(archivedExtra), std::string("archived level"));
	}

	//Exit: the archive namespaces go raw again, every live encoder and table entry is exactly
	//what it was before the replay:
	QCOMPARE(ColumnEncoder::decode(archivedToken), archivedToken);
	QCOMPARE(ColumnEncoder::decode(liveExtraToken), std::string("live level"));
	QCOMPARE(ColumnEncoder::decode(liveToken), std::string("Score"));
	QVERIFY2(canonical->currentNames() == liveColumns,		"live canonical still untouched after the replay");
	QCOMPARE(liveView->encode("Score"), liveToken);
	QCOMPARE(extras.currentNames().size(), size_t(1));

	delete liveView;
	delete canonical;
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

	//mds-plan D3.3 flip: provenance is read from the `.filterIds` sidecar, not from the .meta
	//stamps anymore - and the stamps still sitting in this fixture are now inert input. Make
	//them LIE about the dataset: the encoding and the per-dataset attribution must ignore them
	//completely (the resolver re-attributes dataSetId from the resolved filter).
	options[".filterIds"]["dependent"]	= 11;	//filter 11 lives in dataset 1
	options[".filterIds"]["covariate"]	= 22;	//filter 22 lives in dataset 2
	options[".meta"]["dependent"]["dataSetId"]	= 2;
	options[".meta"]["covariate"]["dataSetId"]	= 1;

	ColumnEncoder::perDataSetColsPlusTypes colsPerDataSet = ColumnEncoder::encodeColumnNamesinOptionsPerDataSet(
			options, true,
			[&dsOne, &dsTwo](int & dataSetId, int filterId) -> ColumnEncoder *
			{
				//mds-plan D3.3/D3.7 resolver shape: the sidecar filterId resolves the encoder AND
				//re-attributes the dataset (filter ids imply their dataset).
				if(filterId == 11)	{ dataSetId = 1; return &dsOne; }
				if(filterId == 22)	{ dataSetId = 2; return &dsTwo; }
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

// mds-plan D1.6/D3.3: the resolver gets the filterId that governs every option node (since the
// D3.3 flip from the `.filterIds` sidecar, before it from the .meta stamp), so two options that
// refer to the same column through two different filters of ONE dataset encode into two distinct
// namespaces - the same two namespaces their slices are handed to R in. An option without a
// sidecar node (or naming a dead filter) must fall back to the dataset-level view the resolver
// picks (C7 legacy).
void TestColumnEncoderContext::encodePerDataSetRoutesOptionsToStampedFilterView()
{
	ColumnEncoder * canonical = ColumnEncoder::createCanonical();
	canonical->setCurrentNames(names({{"Group", columnType::nominal}, {"Score", columnType::scale}}));

	ColumnEncoder * viewShown = ColumnEncoder::createView(canonical, "JASPColumn_D1_F11_");	//the dataset's shown filter
	ColumnEncoder * viewHost  = ColumnEncoder::createView(canonical, "JASPColumn_D1_F12_");	//a form's host filter: NOT shown

	Json::Value options(Json::objectValue);
	options["dependent"]				= Json::Value(Json::objectValue);
	options["dependent"]["value"]		= Json::Value(Json::arrayValue);
	options["dependent"]["value"].append("Score");
	options["dependent"]["types"]		= Json::Value(Json::arrayValue);
	options["dependent"]["types"].append("scale");
	options["splitBy"]					= Json::Value(Json::objectValue);
	options["splitBy"]["value"]			= Json::Value(Json::arrayValue);
	options["splitBy"]["value"].append("Group");
	options["splitBy"]["types"]			= Json::Value(Json::arrayValue);
	options["splitBy"]["types"].append("nominal");
	options["unqualified"]				= Json::Value(Json::objectValue);
	options["unqualified"]["value"]		= Json::Value(Json::arrayValue);
	options["unqualified"]["value"].append("Score");
	options["unqualified"]["types"]		= Json::Value(Json::arrayValue);
	options["unqualified"]["types"].append("scale");

	options[".meta"]["dependent"]["shouldEncode"]	= true;
	options[".meta"]["splitBy"]["shouldEncode"]		= true;
	options[".meta"]["unqualified"]["shouldEncode"]	= true;
	//mds-plan D3.3 flip: the (dataSetId, filterId) stamps move from .meta to the transient
	//`.filterIds` sidecar; an option the sidecar does not name stays unqualified (C7 fallback).
	options[".filterIds"]["dependent"]	= 12;					//the HOST filter, not the shown one
	options[".filterIds"]["splitBy"]	= 11;					//the shown filter
	//no node for "unqualified": resolver fallback, exactly the old no-filterId dual-read

	ColumnEncoder::perDataSetColsPlusTypes colsPerDataSet = ColumnEncoder::encodeColumnNamesinOptionsPerDataSet(
			options, true,
			[&viewShown, &viewHost](int &, int filterId) -> ColumnEncoder *
			{
				//exactly the D3.3 shape of DataBridge::prepareMultiDataSetRun's resolver
				return filterId == 12 ? viewHost : viewShown;
			},
			1);

	QCOMPARE(QString::fromStdString(options["dependent"][0].asString()),  QString::fromStdString(viewHost->encode("Score.scale")));
	QCOMPARE(QString::fromStdString(options["splitBy"][0].asString()),    QString::fromStdString(viewShown->encode("Group.nominal")));
	QCOMPARE(QString::fromStdString(options["unqualified"][0].asString()),QString::fromStdString(viewShown->encode("Score.scale")));

	const std::string dependentToken = options["dependent"][0].asString();
	QVERIFY2(dependentToken.rfind("JASPColumn_D1_F12_", 0) == 0,	qPrintable(QString::fromStdString("stamped option must encode in its filter's namespace, got: " + dependentToken)));
	QVERIFY2(options["splitBy"][0].asString().rfind("JASPColumn_D1_F11_", 0) == 0,	"shown-stamped option encodes in the shown view");

	//Both namespaces are live in the one table, so a single context-free sweep decodes both sides
	//of the same-name collision - the host/shown landmine resolving through the table (C4):
	Json::Value sweep(Json::arrayValue);
	sweep.append(dependentToken);
	sweep.append(options["splitBy"][0].asString());
	ColumnEncoder::decodeJson(sweep);
	QCOMPARE(sweep[0].asString(), std::string("Score"));
	QCOMPARE(sweep[1].asString(), std::string("Group"));

	//Requested cols are still attributed per dataset (the slice queue keys on the filter ids):
	QCOMPARE(colsPerDataSet.size(), size_t(1));
	QVERIFY(colsPerDataSet.count(1) == 1);
	QVERIFY(colsPerDataSet[1].count(std::make_pair(std::string("Score.scale"), columnType::scale)) == 1);
	QVERIFY(colsPerDataSet[1].count(std::make_pair(std::string("Group.nominal"), columnType::nominal)) == 1);

	delete viewHost;
	delete viewShown;
	delete canonical;
}

void TestColumnEncoderContext::encodePerDataSetAttributedToPrimaryWithoutProvenance()
{
	ColumnEncoder dsOne("JASPColumn_1_"), dsTwo("JASPColumn_2_");
	makeTwoDataSets(dsOne, dsTwo);

	Json::Value options = twoDataSetOptions();
	options[".meta"]["covariate"].removeMember("dataSetId"); //mds-plan D3.3: .meta stamps are inert input; this tree carries no `.filterIds` sidecar at all, so EVERY option - named or not - falls back to the primary dataset

	ColumnEncoder::perDataSetColsPlusTypes colsPerDataSet = ColumnEncoder::encodeColumnNamesinOptionsPerDataSet(
			options, true,
			[&dsOne, &dsTwo](int dataSetId, int) -> ColumnEncoder *
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

void TestColumnEncoderContext::collectFilterIdsFromMetaGathersEveryDistinctFilter()
{
	Json::Value meta(Json::objectValue);

	meta["dependentA"]["dataSetId"]	= 3;
	meta["dependentA"]["filterId"]	= 31;
	meta["dependentB"]["dataSetId"]	= 3;              //same dataset...
	meta["dependentB"]["filterId"]	= 32;             //...other filter: a slice of its own
	meta["legacy"]["dataSetId"]		= 4;              //no filterId stamp: contributes nothing (the primary slice is always queued)
	meta["other"]["dataSetId"]		= 4;
	meta["other"]["filterId"]		= 41;             //distinct filter of the same dataset
	meta["nested"][0]["filterId"]	= 31;             //array walk; a duplicate id dedups

	std::set<int> filterIds;
	ColumnEncoder::collectFilterIdsFromMeta(meta, filterIds);

	//Filter ids imply their dataset, so the set of ids IS the set of slices:
	QCOMPARE(filterIds.size(), size_t(3));
	QVERIFY(filterIds.count(31));
	QVERIFY(filterIds.count(32));
	QVERIFY(filterIds.count(41));
}

// The single all-blocks results decoder that Engine::sendString, the syntax bridge, the plotly
// loader and the computed-column site all use now: it resolves tokens minted by the global current
// encoder, the extras encoder (C6) AND every live dataset's filter blocks in ONE context-free
// sweep (decodeJsonAcrossEncoders and its encoder-vector parameter retired at D1.5), and the
// safeHtml switch must be the only difference between desktop (escaped) and bridge/wrapper
// (plain) output.
void TestColumnEncoderContext::decodeJsonCoversEveryBlockAndHonoursSafeHtml()
{
	ColumnEncoder * defaultEncoder = ColumnEncoder::columnEncoder();
	ColumnEncoder::setCurrentEncoder(defaultEncoder);

	ColumnEncoder   extras(ExtraOptionsPrefix);
	ColumnEncoder   dsOne("JASPColumn_1_"), dsTwo("JASPColumn_2_");

	defaultEncoder->setCurrentNames(names({{"global col",	columnType::scale}}));
	extras        .setCurrentNames(names({{"extra col",		columnType::scale}}));
	dsOne         .setCurrentNames(names({{"one col",		columnType::scale}}));
	dsTwo         .setCurrentNames(names({{"two col",		columnType::scale}}));

	Json::Value payload;
	payload["a"] = defaultEncoder->encodeAll("global col");	//global/current encoder's token
	payload["b"] = extras        .encodeAll("extra col");		//extras (a first-class block, C6)
	payload["c"] = dsOne         .encodeAll("one col");			//a dataset block's token
	payload["d"] = dsTwo         .encodeAll("two col");			//...and a second one's

	QVERIFY2(payload["a"].asString() != "global col" && payload["c"].asString() != "one col",
			 "sanity: the inputs must actually be encoded tokens");

	ColumnEncoder::decodeJson(payload);							//one static sweep, no encoder vector

	QCOMPARE(payload["a"].asString(), std::string("global col"));
	QCOMPARE(payload["b"].asString(), std::string("extra col"));
	QCOMPARE(payload["c"].asString(), std::string("one col"));
	QCOMPARE(payload["d"].asString(), std::string("two col"));

	// safeHtml is the only difference: a name with HTML-special characters comes back escaped,
	// plain does not. (Exact escaped form is stringUtils::escapeHtmlStuff's business; we only
	// assert the modes differ and that escaping actually happened.)
	ColumnEncoder ds("JASPColumn_8_");
	ds.setCurrentNames(names({{"Score & Rank [x]", columnType::scale}}));
	const std::string specialToken = ds.encodeAll("Score & Rank [x]");

	Json::Value plainVal, htmlVal;
	plainVal["v"] = specialToken;
	htmlVal["v"]  = specialToken;

	ColumnEncoder::decodeJson(plainVal);
	ColumnEncoder::decodeJson(htmlVal, /*replaceNames=*/true, /*safeHtml=*/true);

	QCOMPARE(plainVal["v"].asString(), std::string("Score & Rank [x]"));
	QVERIFY2(htmlVal["v"].asString() != plainVal["v"].asString(), "safeHtml must escape where plain does not");
	QVERIFY2(htmlVal["v"].asString().find("&amp;") != std::string::npos, "safeHtml should escape the ampersand");

	//Leave the global in a clean state for later tests in this binary.
	defaultEncoder->setCurrentNames(emptyNames());
	ColumnEncoder::setCurrentEncoder(defaultEncoder);
}

// ---------------------------------------------------------------------------------------------
// mds-plan Phase 1: canonical mint + per-filter views + the one global decode table (C1-C4, C16)
// ---------------------------------------------------------------------------------------------

void TestColumnEncoderContext::canonicalAnswersNoFullStringEncodeDecode()
{
	ColumnEncoder * canonical = ColumnEncoder::createCanonical();

	canonical->setCurrentNames(names({{"Group", columnType::nominal}, {"Score", columnType::scale}}));

	// The mint answers membership of original names and holds the type state ...
	QVERIFY2(canonical->shouldEncode("Score"),			"the canonical must know its own column names");
	QVERIFY2(canonical->shouldEncode("Score.nominal"),	"qualified names belong to the mint too");
	QVERIFY2(!canonical->shouldEncode("Nope"),			"unknown names are not members");
	QCOMPARE(canonical->currentNames().size(), size_t(2));

	// ... but full-string encode is not the canonical's job (C2). Decode-side there is nothing left
	// to assert at D1.5: the instance decode API is gone entirely (decode/shouldDecode/
	// columnTypeFromEncoded/decodeAll/decodeJson are static table entries now), so the canonical
	// not answering full-string decode is enforced by the compile itself.
	QVERIFY2(!captureError([&](){ canonical->encode("Score"); }).empty(),	"encode on a canonical must throw");
	QCOMPARE(canonical->encodePrefix(), std::string());	// no namespace of its own

	// The only decode answer for a canonical-looking token is the global table's, and the
	// canonical registers nothing there (C2): without a live view the token is simply unknown.
	QVERIFY2(!ColumnEncoder::shouldDecode("JASPColumn_D1_F11_3_Encoded"),	"a bare canonical registers no block");
	QCOMPARE(ColumnEncoder::decode("JASPColumn_D1_F11_3_Encoded"), std::string("JASPColumn_D1_F11_3_Encoded"));	//raw (C4)

	delete canonical;
}

void TestColumnEncoderContext::filterViewsMintDistinctTokensThatBothDecode()
{
	ColumnEncoder * canonical = ColumnEncoder::createCanonical();
	canonical->setCurrentNames(names({{"Group", columnType::nominal}, {"Score", columnType::scale}}));

	ColumnEncoder * viewOne = ColumnEncoder::createView(canonical, "JASPColumn_D1_F11_");
	ColumnEncoder * viewTwo = ColumnEncoder::createView(canonical, "JASPColumn_D1_F12_");

	const std::string tokenOne = viewOne->encode("Score");
	const std::string tokenTwo = viewTwo->encode("Score");

	// C1 shape + the classic alphabetical mint (Group owns 0-2 with bare->2, Score 3-5 with bare->3):
	QCOMPARE(tokenOne, std::string("JASPColumn_D1_F11_3_Encoded"));
	QCOMPARE(tokenTwo, std::string("JASPColumn_D1_F12_3_Encoded"));
	QVERIFY2(tokenOne != tokenTwo, "the same name in two filters of one dataset must mint distinct tokens");

	// Both decode through the one global table, context-free (C4):
	QCOMPARE(ColumnEncoder::decode(tokenOne), std::string("Score"));
	QCOMPARE(ColumnEncoder::decode(tokenTwo), std::string("Score"));
	QCOMPARE(ColumnEncoder::columnTypeFromEncoded(tokenOne), columnType::scale);
	QVERIFY2(ColumnEncoder::shouldDecode(tokenTwo), "decode is context-free: the table resolves foreign live tokens too");

	// Qualified variants get their own index and type:
	QCOMPARE(viewTwo->encode("Score.nominal"), std::string("JASPColumn_D1_F12_5_Encoded"));
	QCOMPARE(ColumnEncoder::columnTypeFromEncoded(std::string("JASPColumn_D1_F12_5_Encoded")), columnType::nominal);
	QCOMPARE(ColumnEncoder::decode(std::string("JASPColumn_D1_F12_0_Encoded")), std::string("Group"));

	// shouldEncode is canonical membership (C3), not a per-view map:
	QVERIFY2(viewOne->shouldEncode("Group"),	"membership comes from the shared mint");
	QVERIFY2(!viewOne->shouldEncode("Nope"),	"unknown names are not encodable");

	delete viewTwo;
	delete viewOne;
	delete canonical;
}

void TestColumnEncoderContext::remintRewritesEveryViewBlockAndStaleTokensGoRaw()
{
	ColumnEncoder * canonical = ColumnEncoder::createCanonical();
	canonical->setCurrentNames(names({{"Alpha", columnType::scale}, {"Zeta", columnType::scale}}));

	ColumnEncoder * viewOne = ColumnEncoder::createView(canonical, "JASPColumn_D1_F11_");
	ColumnEncoder * viewTwo = ColumnEncoder::createView(canonical, "JASPColumn_D1_F12_");

	const std::string alphaOne	= viewOne->encode("Alpha");
	const std::string zetaOne	= viewOne->encode("Zeta");
	const std::string zetaTwo	= viewTwo->encode("Zeta");
	QCOMPARE(ColumnEncoder::decode(zetaOne), std::string("Zeta"));

	// Column removal (setCurrentNames is the single funnel): the mint shrinks and every view block
	// is rewritten as an automatic consequence - no caller has to remember (D1.4).
	canonical->setCurrentNames(names({{"Alpha", columnType::scale}}));

	QCOMPARE(viewOne->encode("Alpha"), alphaOne);
	QCOMPARE(ColumnEncoder::decode(alphaOne), std::string("Alpha"));
	QCOMPARE(ColumnEncoder::decode(zetaOne), zetaOne);	//stale token: decodes raw
	QCOMPARE(ColumnEncoder::decode(zetaTwo), zetaTwo);	//...in every view of the canonical
	QVERIFY2(!ColumnEncoder::shouldDecode(zetaOne), "shrunk-away tokens must be gone from the table");
	QVERIFY2(!ColumnEncoder::shouldDecode(zetaTwo), "shrunk-away tokens of the other view must be gone too");

	// Rename keeps the token space closed as well: the old "Alpha" name is gone, so its old index
	// now belongs to "Bravo" - the table answers with the CURRENT mapping only, never both.
	canonical->setCurrentNames(names({{"Bravo", columnType::scale}, {"Zeta", columnType::scale}}));
	QCOMPARE(ColumnEncoder::decode(alphaOne), std::string("Bravo"));	//re-minted index: current entry wins
	QCOMPARE(ColumnEncoder::decode(viewOne->encode("Zeta")), std::string("Zeta"));

	delete viewTwo;
	delete viewOne;
	delete canonical;
}

void TestColumnEncoderContext::typeOnlyChangeKeepsTokensAndMovesBareDefaults()
{
	ColumnEncoder * canonical = ColumnEncoder::createCanonical();
	canonical->setCurrentNames(names({{"Score", columnType::scale}}));

	ColumnEncoder * view = ColumnEncoder::createView(canonical, "JASPColumn_D1_F11_");

	const std::string bareToken	= view->encode("Score");			// -> the scale entry
	const std::string ordToken	= view->encode("Score.ordinal");

	// Type-only change (column.cpp:396 funnels here): no re-mint, only the bare-name default moves.
	canonical->updateColumnTypesOnly(names({{"Score", columnType::ordinal}}));

	QCOMPARE(view->encode("Score"), ordToken);							//bare default follows the data type
	QCOMPARE(ColumnEncoder::decode(bareToken), std::string("Score"));	//no token was re-minted
	QCOMPARE(ColumnEncoder::columnTypeFromEncoded(bareToken), columnType::scale);
	QCOMPARE(ColumnEncoder::columnTypeFromEncoded(ordToken), columnType::ordinal);
	QCOMPARE(view->currentNames().at("Score"), columnType::ordinal);		//type state follows (view -> canonical)

	delete view;
	delete canonical;
}

void TestColumnEncoderContext::destroyingViewOrCanonicalErasesBlocks()
{
	ColumnEncoder * canonical = ColumnEncoder::createCanonical();
	canonical->setCurrentNames(names({{"Score", columnType::scale}}));

	ColumnEncoder * doomedView = ColumnEncoder::createView(canonical, "JASPColumn_D1_F42_");
	ColumnEncoder * liveView   = ColumnEncoder::createView(canonical, "JASPColumn_D1_F43_");

	const std::string deadToken = doomedView->encode("Score");
	const std::string liveToken = liveView->encode("Score");

	// Filter destroy: the view dies, its block is erased (C16), the canonical and its other views
	// keep working.
	delete doomedView;

	QCOMPARE(ColumnEncoder::decode(deadToken), deadToken);
	QVERIFY2(!ColumnEncoder::shouldDecode(deadToken), "a destroyed filter's tokens must not decode anymore");
	QCOMPARE(ColumnEncoder::decode(liveToken), std::string("Score"));

	// Dataset unload: ~DataSet deletes the canonical while the filters (as its QObject children)
	// die only later - the canonical must detach and erase every live view first.
	delete canonical;

	QCOMPARE(ColumnEncoder::decode(liveToken), liveToken);
	QVERIFY2(!ColumnEncoder::shouldDecode(liveToken), "a dataset unload must erase the blocks of all its filters");

	delete liveView;	//still alive (like a not-yet-deleted child) but detached - must not crash
}

void TestColumnEncoderContext::independentCanonicalsFromIdenticalMapsProduceIdenticalBlocks()
{
	const ColumnEncoder::colTypeMap columns = names({{"Group", columnType::nominal}, {"Score", columnType::scale}, {"x y", columnType::unknown}});

	ColumnEncoder * canonicalOne = ColumnEncoder::createCanonical();
	ColumnEncoder * canonicalTwo = ColumnEncoder::createCanonical();
	canonicalOne->setCurrentNames(columns);
	canonicalTwo->setCurrentNames(columns);

	ColumnEncoder * viewOne = ColumnEncoder::createView(canonicalOne, "JASPColumn_D7_F70_");
	ColumnEncoder * viewTwo = ColumnEncoder::createView(canonicalTwo, "JASPColumn_D8_F80_");

	// Deterministic minting: identical column maps yield identical indices, so the two processes'
	// (desktop/engine/syntax-bridge analog) blocks agree on every name's index - only the prefix
	// differs - and every token decodes back to the same name and type.
	for(const std::string & name : { std::string("Group"), std::string("Group.scale"), std::string("Score"), std::string("Score.nominal"), std::string("x y") })
	{
		const std::string tokenOne = viewOne->encode(name);
		const std::string tokenTwo = viewTwo->encode(name);

		QVERIFY2(tokenOne.size() > 16 && tokenTwo.size() > 16, "sanity: both must be full tokens");
		QCOMPARE(QString::fromStdString(tokenOne.substr(std::string("JASPColumn_D7_F70_").size())),
		         QString::fromStdString(tokenTwo.substr(std::string("JASPColumn_D8_F80_").size())));	//same index part

		const std::string bareName = name.substr(0, name.find('.'));
		QCOMPARE(ColumnEncoder::decode(tokenOne), bareName);
		QCOMPARE(ColumnEncoder::decode(tokenTwo), bareName);
		QCOMPARE(ColumnEncoder::columnTypeFromEncoded(tokenOne), ColumnEncoder::columnTypeFromEncoded(tokenTwo));
	}

	delete viewTwo;
	delete viewOne;
	delete canonicalTwo;
	delete canonicalOne;
}

void TestColumnEncoderContext::extrasStyleInstanceFeedsTheGlobalTable()
{
	std::string tokenA, tokenB;

	{
		ColumnEncoder extras(ExtraOptionsPrefix);	//a FULL self-contained instance: the extras shape (C6)
		extras.setCurrentNames(names({{"level A", columnType::unknown}, {"level B", columnType::unknown}}));

		tokenA = extras.encode("level A");
		tokenB = extras.encode("level B");

		QCOMPARE(tokenA, std::string("JaspExtraOptions_0_Encoded"));
		QCOMPARE(tokenB, std::string("JaspExtraOptions_1_Encoded"));

		// extras tokens decode at every decode site (context-free):
		QCOMPARE(ColumnEncoder::decode(tokenA), std::string("level A"));
		QCOMPARE(ColumnEncoder::columnTypeFromEncoded(tokenA), columnType::unknown);

		Json::Value payload(Json::objectValue);
		payload["k"] = tokenB;
		ColumnEncoder::decodeJson(payload);
		QCOMPARE(payload["k"].asString(), std::string("level B"));

		// The per-request refresh (setCurrentNamesFromOptionsMeta) closes over the old key space:
		Json::Value options(Json::objectValue);
		options[".meta"]["lv"]["encodeThis"] = "level C";
		extras.setCurrentNamesFromOptionsMeta(options);

		QCOMPARE(ColumnEncoder::decode(tokenA), std::string("level C"));	//index 0 was rewritten
		QVERIFY2(!ColumnEncoder::shouldDecode(tokenB), "index 1 is outside the fresh mint and must be erased, not kept");
	}

	// The extras instance's death erases its whole block.
	QVERIFY2(!ColumnEncoder::shouldDecode(tokenA), "a destroyed extras encoder must leave nothing decodable");
	QVERIFY2(!ColumnEncoder::shouldDecode(tokenB), "a destroyed extras encoder must leave nothing decodable");
}
