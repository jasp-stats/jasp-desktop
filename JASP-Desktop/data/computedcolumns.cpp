#include "computedcolumns.h"
#include "datasetpackage.h"

Columns& ComputedColumns::columns()
{
	return _package->dataSet()->columns();
}

ComputedColumn * ComputedColumns::createComputedColumn(std::string name, Column::ColumnType type, ComputedColumn::computedType desiredType)
{
	Column			* column			= columns().createColumn(name);
	ComputedColumn	* newComputedColumn = new ComputedColumn(&_computedColumns, column, desiredType);

	_computedColumns.push_back(newComputedColumn);
	column->setDefaultValues(type);

	refreshColumnPointers();

	return newComputedColumn;
}

void ComputedColumns::removeComputedColumn(std::string name)
{
	for(auto it=_computedColumns.begin(); it != _computedColumns.end(); it++)
		if((*it)->name() == name)
		{
			delete *it;
			_computedColumns.erase(it);
			break;
		}

	columns().removeColumn(name);  //This moves the columns, meaning the pointers in the other computeColumns are now no longer valid..
	refreshColumnPointers();
}

void ComputedColumns::refreshColumnPointers()
{
	for(ComputedColumn * col : _computedColumns)
		col->setColumn(&(columns().get(col->name())));

	findAllColumnNames();
}

size_t ComputedColumns::findIndexByName(std::string name) const
{
	for(size_t i=0; i<_computedColumns.size(); i++)
		if(_computedColumns[i]->name() == name)
			return i;
	throw std::runtime_error("Cannot find computed column by name: " + name);
}

bool ComputedColumns::setRCode(std::string name, std::string rCode)
{
	try
	{
		return (*this)[name].setRCode(rCode);
	}
	catch(...){}
	return false;
}

bool ComputedColumns::setConstructorJson(std::string name, std::string json)
{
	try
	{
		return (*this)[name].setConstructorJson(json);
	}
	catch(...){}
	return false;
}

bool ComputedColumns::setError(std::string name, std::string error)
{
	try
	{
		return (*this)[name].setError(error);
	}
	catch(...){}
	return false;
}

std::string ComputedColumns::getRCode(std::string name)
{
	try
	{
		return (*this)[name].rCode();
	}
	catch(...){}
	return "";
}

bool ComputedColumns::usesRCode(std::string name)
{
	try
	{
		return (*this)[name].codeType() == ComputedColumn::computedType::rCode;
	}
	catch(...){}
	return "";
}

std::string ComputedColumns::getConstructorJson(std::string name)
{
	try
	{
		return (*this)[name].constructorJson();
	}
	catch(...){}
	return "";
}

std::string ComputedColumns::getError(std::string name)
{
	try
	{
		return (*this)[name].error();
	}
	catch(...){}
	return "";
}

void ComputedColumns::findAllColumnNames()
{
	std::set<std::string> names;

	for(Column & col : columns())
		names.insert(col.name());

	ComputedColumn::setAllColumnNames(names);
}

Json::Value	ComputedColumns::convertToJson()
{
	Json::Value json(Json::arrayValue);

	for(ComputedColumn * col : _computedColumns)
		json.append(col->convertToJson());

	return json;
}

void ComputedColumns::convertFromJson(Json::Value json)
{
	_computedColumns.clear();

	for(auto colJson : json)
		_computedColumns.push_back(new ComputedColumn(&_computedColumns, &columns(), colJson));

	findAllColumnNames();

	for(ComputedColumn * col : _computedColumns)
		col->findDependencies();
}
