#include "label.h"
#include "column.h"
#include "timers.h"
#include "databaseinterface.h"

const int Label::DOUBLE_LABEL_VALUE			= -1; 

Label::Label(Column * column)
: DataSetBaseNode(dataSetBaseNodeType::label, column), _column(column)
{
	_intsId = EmptyValues::missingValueInteger;
}

Label::Label(Column * column, int value)
: DataSetBaseNode(dataSetBaseNodeType::label, column), _column(column)
{
	setIntsId(value);
	setOriginalValue(value);
	setLabel(originalValueAsString());
}

Label::Label(Column * column, const std::string &label, int value, bool filterAllows, const std::string & description, const Json::Value & originalValue, int order, int id)
: DataSetBaseNode(dataSetBaseNodeType::label, column), _column(column)
{
	_label			= label;
	_intsId			= value;
	_filterAllows	= filterAllows;
	_description	= description;//description != "" || label.size() < MAX_LABEL_DISPLAY_LENGTH ? description : label; //Use description given if filled otherwise use label if the label won't be displayed entirely
	_originalValue	= originalValue;
	_order			= order;

	if(id == -1)	dbCreate();
	else			_dbId = id;
}

void Label::dbDelete()
{
	if(_column->batchedLabelDepth())
		return;
	
	assert(_dbId != -1);
	db().labelDelete(_dbId);
	_dbId = -1;
}

void Label::dbCreate()
{
	JASPTIMER_SCOPE(Label::dbCreate);

	if(_column->batchedLabelDepth())
		return;
	
	assert(_dbId == -1);
	_dbId = db().labelAdd(_column->id(), _intsId, _label, _filterAllows, _description, _originalValue.toStyledString());
}

void Label::dbLoad(int labelId)
{
	if(_column->batchedLabelDepth())
		return;
	
	assert(_dbId != -1 || labelId != -1);

	if(labelId != -1)
		_dbId = labelId;

	int columnId;

	std::string origValJsonStr;
	db().labelLoad(labelId, columnId, _intsId, _label, _filterAllows, _description, origValJsonStr, _order);

	_originalValue = Json::nullValue;

	Json::Reader().parse(origValJsonStr, _originalValue);
}

void Label::dbUpdate()
{
	JASPTIMER_SCOPE(Label::dbUpdate);

	if(_column->batchedLabelDepth())
		return;
	
	if(_dbId == -1)
		dbCreate();
	else
	{
		db().labelSet(_dbId, _column->id(), _intsId, _label, _filterAllows, _description, _originalValue.toStyledString());
		_column->incRevision();
	}
}

void Label::setInformation(Column * column, int id, int order, const std::string &label, int value, bool filterAllows, const std::string & description, const Json::Value & originalValue)
{
	assert(_column == column);
	_dbId			= id;
	_order			= order;
	_label			= label;
	_intsId			= value;	
	_filterAllows	= filterAllows;
	_description	= description;
	_originalValue	= originalValue;
}

Json::Value Label::serialize() const
{
	Json::Value json(Json::objectValue);
	
	json["id"]				= _dbId;
	json["order"]			= _order;
	json["label"]			= _label;
	json["intsId"]			= _intsId;
	json["filterAllows"]	= _filterAllows;
	json["description"]		= _description;
	json["originalValue"]	= _originalValue;

	return json;
}

void Label::setIntsId(int value)
{
	_intsId = value;

	dbUpdate();
}

void Label::setOrder(int order)
{
	_order = order;

	//We'll let Column handle the order changes
}

bool Label::setLabel(const std::string & label)
{
	if(_label != label)
	{
		std::string oldLabel = _label;
		_label = label.empty() ? originalValueAsString() : label;
		
		_column->labelDisplayChanged(this, oldLabel);

		dbUpdate();
		return true;
	}
	
	return false;
}

bool Label::setOriginalValue(const Json::Value & originalValue)
{
	if(_originalValue != originalValue)
	{
		Json::Value previous	= _originalValue;
		_originalValue			= originalValue;
		dbUpdate();
		
		_column->labelValueChanged(this, previous);
		
		return true;
	}
	return false;
}

bool Label::setOrigValLabel(const Json::Value &originalValue)
{
	std::string oldLabel	= _label,
				newLabel	= !originalValue.isDouble() ? originalValue.asString() : _column->doubleToDisplayString(originalValue.asDouble(), false, false);
	Json::Value previous	= _originalValue;
	bool		labelChange = _label			!= newLabel,
				valChange	= _originalValue	!= originalValue,
				aChange		= labelChange		|| valChange;
	
	if(labelChange)
		_label = newLabel;
	
	if(valChange)
		_originalValue			= originalValue;
	
	
	if(aChange)
	{
		dbUpdate();
	
		_column->labelValDisplayChanged(this, oldLabel, previous);
		return true;
	}
	return false;
}

bool Label::setDescription(const std::string &description)
{
	if(_description != description)
	{
		_description = description;
		dbUpdate();
		return true;
	}
	return false;
}

bool Label::setFilterAllows(bool allowFilter)
{
	JASPTIMER_SCOPE(Label::setFilterAllows);

	if(_filterAllows != allowFilter)
	{
		_filterAllows = allowFilter;
		dbUpdate();
		return true;
	}
	return false;
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
	this->_intsId			= label._intsId;
	this->_label			= label._label;
	this->_order			= label._order;
	this->_dbId				= label._dbId;
	this->_column			= label._column;
	
	return *this;
}

std::string Label::labelDisplay() const
{
	return isEmptyValue() ? EmptyValues::displayString() : label();
}

std::string Label::labelIgnoreEmpty() const
{
	return label();
}

bool Label::isEmptyValue() const
{
	return _column->isEmptyValue(originalValueAsString(false)) || _column->isEmptyValue(label());
}

std::string Label::originalValueAsString(bool fancyEmptyValue) const
{
	return originalValueAsString(_column, _originalValue, fancyEmptyValue);
}

std::string Label::originalValueAsString(const Column * column, const Json::Value & originalValue, bool fancyEmptyValue)
{
	switch(originalValue.type())
	{
	default:
		return fancyEmptyValue ? EmptyValues::displayString() : "";

	case Json::intValue:
		return std::to_string(originalValue.asInt());

	case Json::realValue:
		return column->doubleToDisplayString(originalValue.asDouble(), fancyEmptyValue);

	case Json::stringValue:
		return originalValue.asString();
	}
}

std::string Label::str() const
{
	return "Label of column '" + _column->name() + "' has display: '" + label() + "' for value " + std::to_string(intsId()) + ", order " + std::to_string(order()) + " and " + ( isEmptyValue() ? "considers itself to be " : "is not ") + "a missing value!";
}

