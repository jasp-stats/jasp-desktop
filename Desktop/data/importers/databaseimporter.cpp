#include "databaseimporter.h"
#include <QSqlRecord>
#include <QSqlField>
#include "database/databaseimportcolumn.h"
#include "utils.h"

ImportDataSet * DatabaseImporter::loadFile(const std::string &locator, boost::function<void(int)> progressCallback)
{
	// locator is the result of DatabaseConnectionInfo::toJson, so:
	Json::Value json;
	if(!Json::Reader().parse(locator, json))
		throw std::runtime_error("DatabaseImporter::loadFile received illegal locator!"); //shouldnt occur normally
	
	_info = DatabaseConnectionInfo(json);
	
	if(!_info.connect())
		throw std::runtime_error(fq(tr("Failed to connect to database %1 at %2 with user %3, last error was: '%4'")
										.arg(_info._database)
										.arg(_info._hostname + ":" + tq(std::to_string(_info._port)))
										.arg(_info._username)
										.arg(_info.lastError())));
	
	
	
	QSqlQuery	query	= _info.runQuery();
	float		progDiv	= 100.0f / float(query.size());
	QSqlRecord  record	= query.record();
	
	ImportDataSet * data = new ImportDataSet(this);

	if(_info._dbType == DbType::QSQLITE)
		query.next(); //skip first empy line
	
	for(int i=0; i<record.count(); i++)
		data->addColumn(new DatabaseImportColumn(data, fq(record.fieldName(i)), record.field(i).metaType()));
												
	long lastProgress = Utils::currentMillis();	
		
	do
	{
		if(lastProgress + 1000 < Utils::currentMillis())
		{
			progressCallback(int(progDiv * query.at()));
			lastProgress = Utils::currentMillis();	
		}

		for(int i=0; i<record.count(); i++)
			static_cast<DatabaseImportColumn*>(data->getColumn(i))->addValue(query.value(i));
	}
	while(query.next());

	_info.close();
	
	return data;
}

void DatabaseImporter::initColumn(QVariant colId, ImportColumn *importColumn)
{
	typedef QMetaType::Type MT;
	
	DatabaseImportColumn * col = static_cast<DatabaseImportColumn*>(importColumn);
	
	switch(col->type().id())
	{
	default:
	case MT::QDate:			//These could later get their own handler, dependent on https://github.com/jasp-stats/INTERNAL-jasp/issues/312 and https://github.com/jasp-stats/jasp-issues/issues/606
	case MT::QDateTime: 
	case MT::Char:
	case MT::QString:
	{
		stringvec asStrings;
		asStrings.reserve(col->size());
		
		for(const QVariant & v : col->getValues())
			asStrings.push_back(fq(v.toString()));
		
		initColumnAsNominalText(colId, col->name(), asStrings);
		break;
	}
		
	case MT::Int:
	case MT::UInt:
	{
		std::vector<int> asInts;
		asInts.reserve(col->size());
		
		for(const QVariant & v : col->getValues())
			asInts.push_back(v.toInt());
		
		initColumnAsNominalOrOrdinal(colId, col->name(), asInts, true);
		break;
	}
		
	case MT::Double:
	case MT::Float:
	{
		std::vector<double> asDoubles;
		asDoubles.reserve(col->size());
		
		for(const QVariant & v : col->getValues())
			asDoubles.push_back(v.toDouble());
		
		initColumnAsScale(colId, col->name(), asDoubles);
		break;
	}
			
	}
}
