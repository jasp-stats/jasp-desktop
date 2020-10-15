#ifndef RSCRIPTSTORE_H
#define RSCRIPTSTORE_H

#include "enginedefinitions.h"
#include "column.h"
#include <QString>

struct RScriptStore
{
	RScriptStore(int requestId, QString script, engineState typeScript = engineState::rCode, bool whiteListedVersion = true, bool returnLog = false)
		: typeScript(typeScript), script(script), requestId(requestId), whiteListedVersion(whiteListedVersion), returnLog(returnLog) {}

	engineState typeScript; //should be filter/rcode/etc
	QString		script;
	int			requestId;
	bool		whiteListedVersion,
				returnLog;

};

struct RFilterStore : public RScriptStore
{
	RFilterStore(QString generatedfilter, QString filter, int requestID) : RScriptStore(requestID, filter, engineState::filter), generatedfilter(generatedfilter) { }

	QString generatedfilter;
};

struct RComputeColumnStore : public RScriptStore
{
	RComputeColumnStore(QString columnName, QString computeCode, columnType colType) : RScriptStore(-1, computeCode, engineState::computeColumn), _columnName(columnName), _columnType(colType)
	{ }

	QString		_columnName;
	columnType	_columnType;
};

#endif // RSCRIPTSTORE_H
