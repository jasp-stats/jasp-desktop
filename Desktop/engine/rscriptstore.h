#ifndef RSCRIPTSTORE_H
#define RSCRIPTSTORE_H

#include "enginedefinitions.h"
#include <QString>


///
/// Base class for storing requests to run some R-script in an engine
struct RScriptStore
{
	RScriptStore(int dataSetId, int requestId, QString script, QString module, engineState typeScript = engineState::rCode, bool whiteListedVersion = true, bool returnLog = false)
		: typeScript(typeScript), script(script), module(module), requestId(requestId), dataSetId(dataSetId), whiteListedVersion(whiteListedVersion), returnLog(returnLog) {}

	engineState typeScript; //should be filter/rcode/etc
	QString		script, module;
	int			requestId,
				dataSetId;
	bool		whiteListedVersion,
				returnLog;

};

///
/// For when you want to run a filter use this override
struct RFilterStore : public RScriptStore
{
	RFilterStore(int dataSetId, QString generatedfilter, QString filter, int requestID) : RScriptStore(dataSetId, requestID, filter, "", engineState::filter), generatedfilter(generatedfilter) { }

	QString generatedfilter;
};

///
/// For when you want to run a filter from a qmlcomponent or something use this override
struct RFilterByNameStore : public RScriptStore
{
	RFilterByNameStore(int dataSetId, int requestId, QString name, QString module) : RScriptStore(dataSetId, requestId, "Filter selected by name", module, engineState::filterByName), name(name) { }

	QString name;
};

///
/// For when a computed column must be, well, computed
struct RComputeColumnStore : public RScriptStore
{
	RComputeColumnStore(int dataSetId, QString columnName, QString computeCode, columnType colType) 
		: RScriptStore(dataSetId, -1, computeCode, "", engineState::computeColumn), _columnName(columnName), _columnType(colType)
	{ }

	QString		_columnName;
	columnType	_columnType;
};

///
/// For when a whole computed dataset must be (re)computed
struct RComputeDataSetStore : public RScriptStore
{
	RComputeDataSetStore(int dataSetId, QString computeCode, int defaultInputFilterId) 
		: RScriptStore(dataSetId, -1, computeCode, "", engineState::computeDataSet), _defaultInputFilterId(defaultInputFilterId)
	{ }

	int			_defaultInputFilterId;
};

#endif // RSCRIPTSTORE_H
