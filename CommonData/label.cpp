#include "label.h"
#include "column.h"
#include <sstream>
#include "databaseinterface.h"
#include "columnutils.h"


Label::Label(Column * column)
: DataSetBaseNode(dataSetBaseNodeType::label, column), _column(column)
{
	_value = std::numeric_limits<int>::lowest();
}

Label::Label(Column * column, int value)
: DataSetBaseNode(dataSetBaseNodeType::label, column), _column(column)
{
	setValue(value);
	setOriginalValue(value);
	setLabel(originalValueAsString());
}

Label::Label(Column * column, const std::string &label, int value, bool filterAllows, const std::string & description, const Json::Value & originalValue, int order, int id)
: DataSetBaseNode(dataSetBaseNodeType::label, column), _column(column)
{
	_label			= label;
	_value			= value;
	_filterAllows	= filterAllows;
	_description	= description != "" || label.size() < MAX_LABEL_DISPLAY_LENGTH ? description : label; //Use description given if filled otherwise use label if the label won't be displayed entirely
	_originalValue	= originalValue;
	_order			= order;

	if(id == -1)	dbCreate();
	else			_id = id;
}

void Label::dbDelete()
{
	if(_column->batchedLabel())
		return;
	
	assert(_id != -1);
	db().labelDelete(_id);
	_id = -1;
}

void Label::dbCreate()
{
	if(_column->batchedLabel())
		return;
		
	assert(_id == -1);
	_id = db().labelAdd(_column->id(), _value, _label, _filterAllows, _description, _originalValue.toStyledString());
}

void Label::dbLoad(int labelId)
{
	if(_column->batchedLabel())
		return;
	
	assert(_id != -1 || labelId != -1);

	if(labelId != -1)
		_id = labelId;

	int columnId;

	std::string origValJsonStr;
	db().labelLoad(labelId, columnId, _value, _label, _filterAllows, _description, origValJsonStr, _order);

	_originalValue = Json::nullValue;

	Json::Reader().parse(origValJsonStr, _originalValue);
}

void Label::dbUpdate()
{
	if(_column->batchedLabel())
		return;
	
	if(_id == -1)
		dbCreate();
	else
	{
		db().labelSet(_id, _column->id(), _value, _label, _filterAllows, _description, _originalValue.toStyledString());
		_column->incRevision();
	}
}

void Label::setInformation(Column * column, int id, int order, const std::string &label, int value, bool filterAllows, const std::string & description, const Json::Value & originalValue)
{
	_id				= id;
	_order			= order;
	_label			= label;
	_value			= value;	
	_filterAllows	= filterAllows;
	_description	= description;
	_originalValue	= originalValue;
}

Json::Value Label::serialize() const
{
	Json::Value json(Json::objectValue);

	json["id"]				= _id;
	json["order"]			= _order;
	json["label"]			= _label;
	json["value"]			= _value;
	json["filterAllows"]	= _filterAllows;
	json["description"]		= _description;
	json["originalValue"]	= _originalValue;

	return json;
}

void Label::setValue(int value)
{
	_value = value;

	dbUpdate();
}

void Label::setOrder(int order)
{
	_order = order;

	//We'll let Column handle the order changes
}

void Label::setOriginalValue(const Json::Value & originalLabel)
{
	if(_originalValue != originalLabel)
	{
		_originalValue = originalLabel;
		dbUpdate();
	}
}

void Label::setDescription(const std::string &description)
{
	if(_description != description)
	{
		_description = description;
		dbUpdate();
	}
}

void Label::setFilterAllows(bool allowFilter)
{
	if(_filterAllows != allowFilter)
	{
		_filterAllows = allowFilter;
		dbUpdate();
	}
}

DatabaseInterface & Label::db()
{
	return _column->db();
}

const DatabaseInterface & Label::db() const
{
	return _column->db();
}

Label &Label::operator=(const Label &label)
{
	this->_originalValue	= label._originalValue;
	this->_filterAllows		= label._filterAllows;
	this->_description		= label._description;
	this->_value			= label._value;
	this->_label			= label._label;
	this->_order			= label._order;
	this->_id				= label._id;

	//this->_column			= label._column; // ???

	return *this;
}

std::string Label::originalValueAsString(bool fancyEmptyValue) const
{
	switch(_originalValue.type())
	{
	default:
		return fancyEmptyValue ? ColumnUtils::emptyValue : "";

	case Json::intValue:
		return std::to_string(_originalValue.asInt());

	case Json::realValue:
		return ColumnUtils::doubleToDisplayString(_originalValue.asDouble(), fancyEmptyValue);

	case Json::stringValue:
		return _originalValue.asString();
	}
}

std::string Label::str() const
{
	return "Label of column '" + _column->name() + "' has display: '" + label() + "' for value " + std::to_string(value()) + " and order " + std::to_string(order());
}

bool Label::setLabel(const std::string & label)
{
	if(_label != label)
	{
		_label = label.empty() ? originalValueAsString() : label;

		dbUpdate();
		return true;
	}
	
	return false;
}
