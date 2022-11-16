#ifndef RSCRIPTSTORE_H
#define RSCRIPTSTORE_H

#include "enginedefinitions.h"
#include "column.h"
#include <QString>


///
/// Base class for storing requests to run some R-script in an engine
struct RScriptStore
{
	RScriptStore(int requestId, QString script, QString module, engineState typeScript = engineState::rCode, bool whiteListedVersion = true, bool returnLog = false)
		: typeScript(typeScript), script(script), module(module), requestId(requestId), whiteListedVersion(whiteListedVersion), returnLog(returnLog) {}

	engineState typeScript; //should be filter/rcode/etc
	QString		script,
				module;
	int			requestId;
	bool		whiteListedVersion,
				returnLog;

};

///
/// For when you want to run a filter use this override
struct RFilterStore : public RScriptStore
{
	RFilterStore(QString generatedfilter, QString filter, int requestID) : RScriptStore(requestID, filter, "", engineState::filter), generatedfilter(generatedfilter) { }

	QString generatedfilter;
};

///
/// For when a computed column must be, well, computed
struct RComputeColumnStore : public RScriptStore
{
	RComputeColumnStore(QString columnName, QString computeCode, columnType colType) : RScriptStore(-1, computeCode, "", engineState::computeColumn), _columnName(columnName), _columnType(colType)
	{ }

	QString		_columnName;
	columnType	_columnType;
};

#endif // RSCRIPTSTORE_H
