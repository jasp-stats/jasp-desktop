#include "desktopcommunicator.h"

DesktopCommunicator::DesktopCommunicator(QObject *parent)
	: QObject{parent}
{
	assert(!_singleton);
	
	_singleton = this;
}

DesktopCommunicator *DesktopCommunicator::singleton()
{
	if(!_singleton)
		new DesktopCommunicator();

	return _singleton;
}

void DesktopCommunicator::setFilterFunctions(
		std::function<void(const std::string&)>						_builderFunc,
		std::function<void(const std::string&)>						_deleteFunc,
		std::function<void(const std::string&, const std::string&)>	_setRFilterFunc,
		std::function<bool(const std::string&)>						_checkForUpdateFunc,
		std::function<std::vector<bool>(const std::string&)>		_filteredFunc,
		std::function<int(const std::string&)>						_filteredRowCountFunc,
		std::function<bool(const std::string&)>						_filterExistFunc)
{
	_filterBuilderFunc			= _builderFunc;
	_filterDeleteFunc			= _deleteFunc;
	_filterSetRFilterFunc		= _setRFilterFunc;
	_filterCheckForUpdateFunc	= _checkForUpdateFunc;
	_filterFilteredFunc			= _filteredFunc;
	_filterFilteredRowCountFunc	= _filteredRowCountFunc;
	_filterNameExistFunc		= _filterExistFunc;
}

void DesktopCommunicator::filterBuild(const std::string &name)
{
	_filterBuilderFunc(name);
}

void DesktopCommunicator::filterDelete(const std::string& name)
{
	_filterDeleteFunc(name);
}

void DesktopCommunicator::filterSetRScript(const std::string& name, const std::string& rFilter)
{
	_filterSetRFilterFunc(name, rFilter);
}

bool DesktopCommunicator::filterCheckForUpdate(const std::string& name)
{
	return _filterCheckForUpdateFunc(name);
}

std::vector<bool> DesktopCommunicator::filtered(const std::string& name)
{
	return _filterFilteredFunc(name);
}

int DesktopCommunicator::filteredRowCount(const std::string& name)
{
	return _filterFilteredRowCountFunc(name);
}

bool DesktopCommunicator::filterNameIsFree(const std::string &name)
{
	return _filterNameExistFunc(name);
}

bool DesktopCommunicator::useNativeFileDialog()
{
#ifdef BUILDING_JASP
	return emit useNativeFileDialogSignal();
#else
	return true;
#endif
}

DesktopCommunicator * DesktopCommunicator::_singleton = nullptr;
