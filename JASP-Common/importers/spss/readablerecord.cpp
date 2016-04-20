#include "readablerecord.h"

using namespace std;
using namespace spss;

set<RecordRoot *> *RecordRoot::_pRecords = 0;

RecordRoot::RecordRoot()
{

	if (_pRecords == 0)
		_pRecords = new set<RecordRoot *>();

	// Keep track of instances.
	_pRecords->insert(this);
}


RecordRoot::~RecordRoot()
{
	set<RecordRoot *>::iterator iter = _pRecords->find(this);
	if ( iter != _pRecords->end())
		_pRecords->erase(iter);

	if (_pRecords->size() == 0)
	{
		delete _pRecords;
		_pRecords = 0;
	}
}

/**
 * @brief processAllStrings Calls processStrings(const SpssCPConvert) on all memeber of _records.
 * @param converter The convertor to pass on.
 */
void RecordRoot::processAllStrings(const SpssCPConvert &converter)
{
	for(set<RecordRoot *>::iterator iter = _pRecords->begin();
		iter != _pRecords->end();
		++iter)
	{
		(*iter)->processStrings(converter);
	}

}
