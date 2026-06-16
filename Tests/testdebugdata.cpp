#include "testinfo.h"
#include "tempfiles.h"
#include "columnutils.h"
#include "processinfo.h"
#include "testdebugdata.h"
#include "utilities/qutils.h"
#include "databaseinterface.h"
#include "utilities/settings.h"
#include "data/datasetpackage.h"
#include "data/importers/csvimporter.h"

void TestDebugData::initTestCase()
{
	TempFiles::init(ProcessInfo::currentPID()); // needed here so that the LRNAM can be passed the session directory

}

void TestDebugData::init()
{
	TempFiles::clearSessionDir();
	Settings::informSettingsThatThisIsATest();
	
	_pkg		= new DataSetPackage(this);
	_importer	= new CSVImporter();
	
	_importer->loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), [](int i){});

	_data = _pkg->dataSet();
	
	Column * facFive = _data->column("facFive");
	
	if(!facFive->hasLabels())
		facFive->noLabelsToLabels();
}

void TestDebugData::cleanup()
{
	if(_data)
		delete _data;
	_data = nullptr;

	DatabaseInterface::singleton()->close();
	DatabaseInterface::singleton()->closeInterfaces();
	
	if(_pkg)
		delete _pkg;
	_pkg = nullptr;
	
	if(_importer)
		delete _importer;
	_importer = nullptr;
}


void TestDebugData::testReverseNumericals()
{
	QVERIFY2(_data,						"No dataset!");
	
	Column * facFive = _data->column("facFive");
	
	QVERIFY2(facFive,						"No facFive!");
	
	Json::Value labelsBefore =	facFive->serializeLabels(true);
								facFive->valuesReverse();
	Json::Value labelsAfter1 =	facFive->serializeLabels(true);
								facFive->valuesReverse();
	Json::Value labelsAfter2 =	facFive->serializeLabels(true);
	
	QVERIFY2(labelsBefore == labelsAfter2,		"Reversing values is not reversible!");
	QVERIFY2(labelsBefore != labelsAfter1,		"Reversing values does not change the labels!");
	
	const std::string jsonReversed = R"Something(
[
	{
		"description" : "",
		"filterAllows" : true,
		"label" : "5",
		"order" : 0,
		"originalValue" : "1"
	},
	{
		"description" : "",
		"filterAllows" : true,
		"label" : "4",
		"order" : 1,
		"originalValue" : "2"
	},
	{
		"description" : "",
		"filterAllows" : true,
		"label" : "",
		"order" : 2,
		"originalValue" : "3"
	},
	{
		"description" : "",
		"filterAllows" : true,
		"label" : "2",
		"order" : 3,
		"originalValue" : "4"
	},
	{
		"description" : "",
		"filterAllows" : true,
		"label" : "1",
		"order" : 4,
		"originalValue" : "5"
	}
]
)Something";
	
	Json::Value hardcoded;
	Json::Reader parser;
	
	parser.parse(jsonReversed, hardcoded);
	
	if(hardcoded != labelsAfter1)
		std::cerr << labelsAfter1 << std::endl;
	
	QVERIFY2(hardcoded == labelsAfter1,		"Reversing values is not right!");
	
	
	DataSet loadMe(_data->id());
	QVERIFY2(_data->jsonForCompare() == loadMe.jsonForCompare(), "DataSet isnt the same after dbload!");
	
}


void TestDebugData::testReverseLabels()
{
	QVERIFY2(_data,		"No dataset!");
	
	Column * facFive = _data->column("facFive");
	
	QVERIFY2(facFive,	"No facFive!");
	
	facFive->setAutoSortByValue(false);
	
	Json::Value labelsBefore =	facFive->serializeLabels(true);
								facFive->labelsReverse();
	Json::Value labelsAfter1 =	facFive->serializeLabels(true);
								facFive->labelsReverse();
	Json::Value labelsAfter2 =	facFive->serializeLabels(true);
	
	QVERIFY2(labelsBefore == labelsAfter2,		"Reversing labels is not reversible!");
	QVERIFY2(labelsBefore != labelsAfter1,		"Reversing labels does not change the labels!");
	
	const std::string jsonReversed = R"Something(
[
	{
		"description" : "",
		"filterAllows" : true,
		"label" : "",
		"order" : 0,
		"originalValue" : "5"
	},
	{
		"description" : "",
		"filterAllows" : true,
		"label" : "",
		"order" : 1,
		"originalValue" : "4"
	},
	{
		"description" : "",
		"filterAllows" : true,
		"label" : "",
		"order" : 2,
		"originalValue" : "3"
	},
	{
		"description" : "",
		"filterAllows" : true,
		"label" : "",
		"order" : 3,
		"originalValue" : "2"
	},
	{
		"description" : "",
		"filterAllows" : true,
		"label" : "",
		"order" : 4,
		"originalValue" : "1"
	}
]
)Something";
	
	Json::Value hardcoded;
	Json::Reader parser;
	
	parser.parse(jsonReversed, hardcoded);
	
	QVERIFY2(hardcoded == labelsAfter1,		"Reversing values is not right!");
	
	if(hardcoded != labelsAfter1)
		std::cerr << labelsAfter1 << std::endl;
	
	DataSet loadMe(_data->id());
	QVERIFY2(_data->jsonForCompare() == loadMe.jsonForCompare(), "DataSet isnt the same after dbload!");
}

void TestDebugData::testColumnStuff()
{
	QVERIFY2(_data,		"No dataset!");
	
	auto originalColumnCount = _data->columnCount();
	auto originalColumnNames = _data->getColumnNames();
	
	stringset	removeUs = { "debInf", "contcor1", "contcor2", "debString" },
				oriNames = stringset(originalColumnNames.begin(), originalColumnNames.end());
	
	for(auto & remMe : removeUs)
	{
		QVERIFY2(oriNames.count(remMe),		"Test column to remove doesnt exist");
		QVERIFY2(_data->column(remMe),		"Looking up column failed");
		
		_data->removeColumn(remMe);
		QVERIFY2(_data->columnCount() == originalColumnCount - 1,	"Columncount didnt decrease");
		
		originalColumnCount = _data->columnCount();
		
		QVERIFY2(!_data->column(remMe),		"Column is still there!");
	}
	
	Column * V1 = _data->column("V1");
	QVERIFY2(V1, "Column V1 is missing...");
	
	V1->setName("Variable 1");
	QVERIFY2(V1->name()  == "Variable 1", "Rename failed");
	QVERIFY2(V1->title() == "Variable 1", "Rename failed to also change the title");
	
	Column * Var1 = _data->column("Variable 1");
	QVERIFY2(Var1, "Rename column didnt update the dataset lookup");
	QVERIFY2(Var1 == V1, "Renamed column is not the same column");
	
	V1->setTitle("Something else entirely");
	QVERIFY2(V1->title() == "Something else entirely", "Retitle failed");
	
	V1->setName("Var1");
	QVERIFY2(V1->name()  == "Var1", "Rename failed");
	QVERIFY2(V1->title() == "Something else entirely", "Got retitled even though it shouldnt have been!");
	
	V1->setTitle("Var1");
	QVERIFY2(V1->title() == "Var1", "Retitle failed");
	
	V1->setName("Variable 1");
	QVERIFY2(V1->name()  == "Variable 1", "Rename failed");
	QVERIFY2(V1->title() == "Variable 1", "Rename failed to also change the title");

	
	DataSet loadMe(_data->id());
	QVERIFY2(_data->jsonForCompare() == loadMe.jsonForCompare(), "DataSet isnt the same after dbload!");
	
	
	
}

void TestDebugData::testEmptyValues()
{
	QVERIFY2(_data,		"No dataset!");
	
	Column * contBinom = _data->column("contBinom");
	
	if(!contBinom->hasLabels())
		contBinom->noLabelsToLabels();
	
	QVERIFY2(contBinom->nonEmptyLevelsStrings().size() == 2, "Not right amount of non-empty labels!");
	
	contBinom->setHasCustomEmptyValues(true);
	contBinom->setCustomEmptyValues({"1"});
	QVERIFY2(contBinom->nonEmptyLevelsStrings().size() == 1,	"Not right amount of non-empty labels after adding one!");
	QVERIFY2(contBinom->nonEmptyLevelsStrings()[0] == "0",		"Not right non-empty label left after adding one empty value!");
	
	contBinom->setCustomEmptyValues({"0"});
	QVERIFY2(contBinom->nonEmptyLevelsStrings().size() == 1,	"Not right amount of non-empty labels after changing one empty value into another!");
	QVERIFY2(contBinom->nonEmptyLevelsStrings()[0] == "1",		"Not right non-empty label left after adding one empty value!");

	
	contBinom->setCustomEmptyValues({"0", "1"});
	QVERIFY2(contBinom->nonEmptyLevelsStrings().size() == 0,	"There should be no labels anymore!");
	
	contBinom->setHasCustomEmptyValues(false);
	QVERIFY2(contBinom->nonEmptyLevelsStrings().size() == 2,	"Not right amount of non-empty labels after disabling custom empty values!");
	
	_data->setWorkspaceEmptyValues({"1"});
	QVERIFY2(contBinom->nonEmptyLevelsStrings().size() == 1,	"Not right amount of non-empty labels after adding one empty value to workspace!");
	QVERIFY2(contBinom->nonEmptyLevelsStrings()[0] == "0",		"Not right non-empty label left after adding one empty value to workspace!");
	
	contBinom->setHasCustomEmptyValues(true);
	contBinom->setCustomEmptyValues({"0"});
	QVERIFY2(contBinom->nonEmptyLevelsStrings().size() == 0,	"There should be no labels anymore!");
}

void TestDebugData::testChangeLabelValueTwice()
{
	QVERIFY2(_data,		"No dataset!");

	Column * contBinom = _data->column("contBinom");
	QVERIFY2(contBinom,					"No contBinom!");
	QVERIFY2(contBinom->hasLabels(),	"contBinom should have labels");
	QVERIFY2(contBinom->labels().size() >= 1, "contBinom has no labels!");

	Label * lbl = contBinom->labels()[0];

	// First value change: numeric → non-numeric string (goes through labelValDisplayChanged)
	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(lbl), "hello", int(DataSetPackage::specialRoles::value));
	QVERIFY2(lbl->originalValueAsString() == "hello",	"First value change to 'hello' failed");
	QVERIFY2(lbl->label() == "hello",	"Lable value change to 'hello' failed");

	// Second value change: non-numeric string → numeric (goes through labelValueChanged,
	// which uses lastOrigValDisplay().
	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(lbl), "5", int(DataSetPackage::specialRoles::value));
	QVERIFY2(lbl->originalValueAsString() == "5",		"Second value change to '5' failed");
	QVERIFY2(lbl->label() == "hello",		"Label value should stay to 'hello' failed");

	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(lbl), "6", int(DataSetPackage::specialRoles::value));
	QVERIFY2(lbl->originalValueAsString() == "6",		"Third value change to '6' failed");
	QVERIFY2(lbl->label() == "hello",		"Label value should stay to 'hello' failed");

	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(lbl), "6", int(DataSetPackage::specialRoles::label));
	QVERIFY2(lbl->originalValueAsString() == "6",		"Label value should stay to '6' failed");
	QVERIFY2(lbl->label() == "6",		"Label change to '6' failed");

	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(lbl), "7", int(DataSetPackage::specialRoles::label));
	QVERIFY2(lbl->originalValueAsString() == "6",		"Label value should stay to '6' failed");
	QVERIFY2(lbl->label() == "7",		"Label change to '7' failed");
	
	QVERIFY2(lbl->label() != lbl->originalValueAsString(), "value and label ought to be different!");
	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(lbl), "7", int(DataSetPackage::specialRoles::value));
	QVERIFY2(lbl->label() == lbl->originalValueAsString(), "value and label ought to be same now!");

	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(lbl), "8", int(DataSetPackage::specialRoles::value));
	QVERIFY2(lbl->originalValueAsString() == "8",		"Label value change to '8' failed'");
	QVERIFY2(lbl->label() == "8",		"Label value change to '8' failed'");

	DataSet loadMe(_data->id());
	QVERIFY2(_data->jsonForCompare() == loadMe.jsonForCompare(), "DataSet isnt the same after dbload!");
}

void TestDebugData::testChangeLabel()
{
	QVERIFY2(_data,		"No dataset!");
	
	Column * contBinom = _data->column("contBinom");
	
	QVERIFY2(contBinom->hasLabels(),							"contBinom should have labels");
	
	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(contBinom->labels()[0]), "A", int(DataSetPackage::specialRoles::label));
	
	QVERIFY2(contBinom->labels()[0]->labelDisplay() == "A",		"contBinom failed renaming first label to A");
	
	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(contBinom->labels()[1]), "B", int(DataSetPackage::specialRoles::value));
	
	QVERIFY2(contBinom->labels()[1]->labelDisplay() == "B",		"contBinom failed renaming first value (and thus also label!) to B");
	
}

void TestDebugData::testShadowDisplay()
{
	QVERIFY2(_data,		"No dataset!");
	
	Column * contNormal = _data->column("contNormal");
	
	QVERIFY2(contNormal->type() == columnType::scale,			"contNormal should be scale type");
	
	contNormal->labelsToNoLabels();
	
	QVERIFY2(!contNormal->hasLabels(),							"contNormal should not have labels");
	
	for (size_t row = 0; row < 10; row++)
	{
		std::string value = contNormal->getValue(row, false, true);
		std::string display = contNormal->getDisplay(row, false, false);
		std::string shadow = contNormal->getShadow(row, false, false);
		
	QString shadMsg = QString("Row %1: Shadow should not be empty when display would be empty! (value='%2', display='%3', shadow='%4')")
				.arg(row).arg(QString::fromStdString(value)).arg(QString::fromStdString(display)).arg(QString::fromStdString(shadow));
		std::string shadMsgStr = shadMsg.toStdString();
		QVERIFY2(!shadow.empty() || value.empty(), shadMsgStr.c_str());
	}
	
	Column * contWide = _data->column("contWide");
	
	QVERIFY2(contWide->type() == columnType::scale,			"contWide should be scale type");
	
	contWide->labelsToNoLabels();
	
	QVERIFY2(!contWide->hasLabels(),							"contWide should not have labels");
	
	std::string val5 = contWide->getValue(5, false, true);
	std::string disp5 = contWide->getDisplay(5, false, false);
	std::string shad5 = contWide->getShadow(5, false, false);
	
	QString shad5Msg = QString("contWide row 5 shadow should not be empty (val='%1', disp='%2', shad='%3')")
				.arg(val5.c_str()).arg(disp5.c_str()).arg(shad5.c_str());
		std::string shad5MsgStr = shad5Msg.toStdString();
		QVERIFY2(!shad5.empty(), shad5MsgStr.c_str());
	
	Column * contBinom = _data->column("contBinom");
	
	if(!contBinom->hasLabels())
		contBinom->noLabelsToLabels();
	
	QVERIFY2(contBinom->hasLabels(),							"contBinom should have labels now");
	
	std::string valBinom0 = contBinom->getValue(0, false, true);
	std::string dispBinom0 = contBinom->getDisplay(0, false, false);
	std::string shadBinom0 = contBinom->getShadow(0, false, false);
	
	QString shadBinom0Msg = QString("contBinom row 0 shadow should not be empty (val='%1', disp='%2', shad='%3')")
				.arg(valBinom0.c_str()).arg(dispBinom0.c_str()).arg(shadBinom0.c_str());
		std::string shadBinom0MsgStr = shadBinom0Msg.toStdString();
		QVERIFY2(!shadBinom0.empty(), shadBinom0MsgStr.c_str());
	
	Column * contGamma = _data->column("contGamma");
	
	QVERIFY2(contGamma->type() == columnType::scale,			"contGamma should be scale type");
	
	contGamma->labelsToNoLabels();
	
	QVERIFY2(!contGamma->hasLabels(),							"contGamma should not have labels");
	
	std::string valContGamma = contGamma->getValue(0, false, true);
	std::string dispContGamma = contGamma->getDisplay(0, false, false);
	std::string shadContGamma = contGamma->getShadow(0, false, false);
	
	QString shadContGammaMsg = QString("contGamma row 0 shadow should not be empty (val='%1', disp='%2', shad='%3')")
				.arg(valContGamma.c_str()).arg(dispContGamma.c_str()).arg(shadContGamma.c_str());
	
	QVERIFY2(!shadContGamma.empty(), shadContGammaMsg.toStdString().c_str());
	
}

void TestDebugData::testValueEqualsDisplayStorage()
{
	QVERIFY2(_data,		"No dataset!");
	
	Column * contBinom = _data->column("contBinom");
	
	if(!contBinom->hasLabels())
		contBinom->noLabelsToLabels();
	
	Label * label = contBinom->labels()[0];
	std::string currentVal = label->originalValueAsString();
	
	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(label), currentVal.c_str(), int(DataSetPackage::specialRoles::label));
	
	double dblVal;
	bool isNumeric = !std::isnan(ColumnUtils::getDoubleValue(currentVal, dblVal)) && !std::isnan(dblVal);
	
	if(isNumeric)
	{
		QVERIFY2(label->label(false)   == "", "Numeric value with same display should have empty label stored");
		QVERIFY2(label->labelDisplay() == currentVal, "Display should show the value");
	}
	else
	{
		QVERIFY2(label->label(false)   != "", "Non-numeric should have non-empty label");
		QVERIFY2(label->labelDisplay() == label->label(), "Display should match label");
	}
	
	DataSet loadMe(_data->id());
	QVERIFY2(_data->jsonForCompare() == loadMe.jsonForCompare(), "DataSet isnt the same after dbload!");
}

void TestDebugData::testSequentialValueChanges()
{
	QVERIFY2(_data,		"No dataset!");
	
	Column * contBinom = _data->column("contBinom");
	
	if(!contBinom->hasLabels())
		contBinom->noLabelsToLabels();
	
	Label * label = contBinom->labels()[0];
	
	std::string firstVal = label->originalValueAsString();
	
	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(label), "test1", int(DataSetPackage::specialRoles::value));
	QVERIFY2(label->originalValueAsString() == "test1", "First value change failed");
	
	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(label), "test2", int(DataSetPackage::specialRoles::value));
	QVERIFY2(label->originalValueAsString() == "test2", "Second value change failed");
	
	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(label), "test3", int(DataSetPackage::specialRoles::value));
	QVERIFY2(label->originalValueAsString() == "test3", "Third value change failed");
	
	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(label), "5", int(DataSetPackage::specialRoles::value));
	QVERIFY2(label->originalValueAsString() == "5", "String to numeric conversion failed");
	
	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(label), "6", int(DataSetPackage::specialRoles::value));
	QVERIFY2(label->originalValueAsString() == "6", "Numeric to numeric conversion failed");
	
	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(label), "hello", int(DataSetPackage::specialRoles::value));
	QVERIFY2(label->originalValueAsString() == "hello", "Numeric to string conversion failed");
	
	DataSet loadMe(_data->id());
	QVERIFY2(_data->jsonForCompare() == loadMe.jsonForCompare(), "DataSet isnt the same after dbload!");
}

void TestDebugData::testEmptyValueLabel()
{
	QVERIFY2(_data,		"No dataset!");
	
	Column * contBinom = _data->column("contBinom");
	
	if(!contBinom->hasLabels())
		contBinom->noLabelsToLabels();
	
	Label * label = contBinom->labels()[0];
	
	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(label), "", int(DataSetPackage::specialRoles::value));
	
	QVERIFY2(label->originalValueAsString() == "", "Empty value should be stored");
	
	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(label), "", int(DataSetPackage::specialRoles::label));
	
	QVERIFY2(label->label() == "", "Empty label should be stored");
	
	DataSet loadMe(_data->id());
	QVERIFY2(_data->jsonForCompare() == loadMe.jsonForCompare(), "DataSet isnt the same after dbload!");
}

void TestDebugData::testNumericToStringConversion()
{
	QVERIFY2(_data,		"No dataset!");
	
	Column * contBinom = _data->column("contBinom");
	
	if(!contBinom->hasLabels())
		contBinom->noLabelsToLabels();
	
	Label * label = contBinom->labels()[0];
	
	std::string original = label->originalValueAsString();
	
	double dbl;
	bool wasNumeric = !std::isnan(ColumnUtils::getDoubleValue(original, dbl));
	
	if(wasNumeric)
	{
		DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(label), "newstring", int(DataSetPackage::specialRoles::value));
		
		QVERIFY2(label->originalValueAsString() == "newstring", "Numeric to string conversion failed");
		QVERIFY2(label->label() == "newstring", "Label should match new value");
		
		DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(label), "5.5", int(DataSetPackage::specialRoles::value));
		
		QVERIFY2(label->originalValueAsString() == "5.5", "String to double conversion failed");
	}
	
	DataSet loadMe(_data->id());
	QVERIFY2(_data->jsonForCompare() == loadMe.jsonForCompare(), "DataSet isnt the same after dbload!");
}

void TestDebugData::testStringToNumericConversion()
{
	QVERIFY2(_data,		"No dataset!");
	
	Column * contBinom = _data->column("contBinom");
	
	if(!contBinom->hasLabels())
		contBinom->noLabelsToLabels();
	
	Label * label = contBinom->labels()[0];
	
	std::string original = label->originalValueAsString();
	
	double dbl;
	bool wasNumeric = !std::isnan(ColumnUtils::getDoubleValue(original, dbl));
	
	if(!wasNumeric)
	{
		DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(label), "42", int(DataSetPackage::specialRoles::value));
		
		QVERIFY2(label->originalValueAsString() == "42", "String to numeric conversion failed");
		
		DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(label), "100", int(DataSetPackage::specialRoles::value));
		
		QVERIFY2(label->originalValueAsString() == "100", "Sequential numeric changes failed");
	}
	
	DataSet loadMe(_data->id());
	QVERIFY2(_data->jsonForCompare() == loadMe.jsonForCompare(), "DataSet isnt the same after dbload!");
}

void TestDebugData::testBatchOperationsWithFilters()
{
	QVERIFY2(_data,		"No dataset!");
	
	Column * facFive = _data->column("facFive");
	
	if(!facFive->hasLabels())
		facFive->noLabelsToLabels();
	
	facFive->setAutoSortByValue(false);
	
	Labelset labelsToDisable;
	int count = 0;
	for(Label * label : facFive->labels())
	{
		if(!label->isEmptyValue() && count < 2)
		{
			labelsToDisable.insert(label);
			count++;
		}
	}
	
	for(Label * label : labelsToDisable)
		label->setFilterAllows(false);
	
	QVERIFY2(facFive->hasFilter(), "Filter should be active");
	
	facFive->valuesReverse();
	
	for(Label * label : labelsToDisable)
		QVERIFY2(!label->filterAllows(), "Filter allows should persist after reverse");
	
	facFive->resetFilter();
	
	QVERIFY2(facFive->allLabelsPassFilter(), "All labels should pass after reset");
	
	DataSet loadMe(_data->id());
	QVERIFY2(_data->jsonForCompare() == loadMe.jsonForCompare(), "DataSet isnt the same after dbload!");
}


// The following test would require actually using the undo model commands
//void TestDebugData::testUndoRedoAfterLabelChanges()
//{
//	QVERIFY2(_data,		"No dataset!");
//	
//	Column * contBinom = _data->column("contBinom");
//	
//	if(!contBinom->hasLabels())
//		contBinom->noLabelsToLabels();
//	
//	Label * label = contBinom->labels()[0];
//	
//	std::string originalLabel = label->label();
//	std::string originalValue = label->originalValueAsString();
//	
//	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(label), "NewLabel", int(DataSetPackage::specialRoles::label));
//	QVERIFY2(label->label() == "NewLabel", "Label should change");
//	
//	DataSetPackage::pkg()->undoStack()->undo();
//	QVERIFY2(label->label() == originalLabel, "Undo should restore original label");
//	
//	DataSetPackage::pkg()->undoStack()->redo();
//	QVERIFY2(label->label() == "NewLabel", "Redo should reapply label change");
//	
//	DataSetPackage::pkg()->setData(DataSetPackage::pkg()->indexForSubNode(label), "NewValue", int(DataSetPackage::specialRoles::value));
//	QVERIFY2(label->originalValueAsString() == "NewValue", "Value should change");
//	
//	DataSetPackage::pkg()->undoStack()->undo();
//	QVERIFY2(label->originalValueAsString() == originalValue, "Undo should restore original value");
//	
//	DataSetPackage::pkg()->undoStack()->redo();
//	QVERIFY2(label->originalValueAsString() == "NewValue", "Redo should reapply value change");
//	
//	DataSet loadMe(_data->id());
//	QVERIFY2(_data->jsonForCompare() == loadMe.jsonForCompare(), "DataSet isnt the same after dbload!");
//}

QTEST_MAIN(TestDebugData)
