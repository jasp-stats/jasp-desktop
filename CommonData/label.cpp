#include "label.h"
#include "column.h"
#include <cassert>
#include "timers.h"
#include "qutils.h"
#include "dataenums.h"
#include "columnutils.h"
#include "databaseinterface.h"

const int Label::NO_LABEL			= -1; 

Label::Label(Column * column, const std::string &label, int value, bool filterAllows, const std::string & description, const Json::Value & originalValue, int order, int id)
: DataSetBaseNode(dataSetBaseNodeType::label, column), _column(column)
{
	setInformation(column, id, order, label, value, filterAllows, description, originalValue);

	if(id == -1)	dbCreate();
	else			_dbId = id;
	
	connect(this, &Label::labelFilterChanged,	column, &Column::labelFilterChanged);
	connect(this, &Label::manualEditMade,		column, &Column::manualEditMade);
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
	db().labelLoad(labelId, columnId, _intsId, _label, _filterAllows, _description, origValJsonStr, _order, _userAdded);

	Json::Value originalValue = Json::nullValue;
	Json::Reader().parse(origValJsonStr, originalValue);
	_setOriginalValue(originalValue);
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
		db().labelSet(_dbId, _column->id(), _intsId, _label, _filterAllows, _description, _originalValue.toStyledString(), _userAdded);
		_column->incRevision();
	}
}

void Label::setInformation(Column * column, int id, int order, const std::string &label, int value, bool filterAllows, const std::string & description, const Json::Value & originalValue)
{
	assert(_column == column);

	_setOriginalValue(originalValue);

	_intsId			= value;
	_filterAllows	= filterAllows;
	_description	= description;
	_order			= order;

	std::string oriStr = originalValueAsString();
	_label			= std::isnan(originalValueAsDouble()) || label != oriStr ? label : ""; // dont store a label if its simply the double
}

std::string Label::processLabel(const std::string & label, const std::string & value)
{
	double dbl;
	
	ColumnUtils::getDoubleValue(value, dbl);
			
	return std::isnan(dbl) || label != value ? label : "";
}

void Label::rememberCurrentOrigValDisplay()
{
	_lastValDisMapping = origValDisplay();
}

Json::Value Label::serialize(bool forCompare) const
{
	Json::Value json(Json::objectValue);
	
	if(!forCompare)
	{
		json["id"]			= _dbId;
		json["intsId"]		= _intsId;
	}
	
	json["order"]			= _order;
	json["label"]			= _label;
	json["filterAllows"]	= _filterAllows;
	json["description"]		= _description;
	json["originalValue"]	= !forCompare ? _originalValue : originalValueAsString();

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
		_label = label;
		
		_column->labelDisplayChanged(this, oldLabel);

		dbUpdate();
		
		emit _column->labelChanged(_column, tq(oldLabel), tq(_label));
		return true;
	}
	
	return false;
}

void Label::_setOriginalValue(const Json::Value & originalValue)
{
	_originalValue			= originalValue;
	
	if(_originalValue.toStyledString() == "null\n")
		_originalValue		= ""; //NaN gets converted into null by json cause the format sucks
		
	ColumnUtils::getDoubleValue(originalValueAsString(false, true), _dblValue);
}


bool Label::setOriginalValue(const Json::Value & originalValue)
{
	if(_originalValue != originalValue)
	{
		Json::Value previous	= _originalValue;
		
		_setOriginalValue(originalValue);
		
		dbUpdate();
		
		_column->labelValueChanged(this, previous);
		
		return true;
	}
	return false;
}

bool Label::setOrigValLabel(const Json::Value &originalValue)
{
	std::string oldLabel	= _label,
				newLabel	= !originalValue.isDouble() ? originalValue.asString() : "";
	Json::Value previous	= _originalValue;
	bool		labelChange = _label			!= newLabel || previous != originalValue, //If they are both "" it could still be a change because the originalValue apparently changed and that is what is shown
				valChange	= _originalValue	!= originalValue,
				aChange		= labelChange		|| valChange;
	
	if(labelChange)
		_label = newLabel;
	
	if(valChange)
		_setOriginalValue(originalValue);
	
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

		emit labelFilterChanged();

		return true;
	}
	return false;
}

void Label::setUserAdded(bool userAddedIt) 
{ 	
	if(_userAdded != userAddedIt)
	{
		_userAdded = userAddedIt;
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
	this->_dblValue			= label._dblValue;
	this->_intsId			= label._intsId;
	this->_label			= label._label;
	this->_order			= label._order;
	this->_dbId				= label._dbId;
	this->_column			= label._column;
	
	return *this;
}

std::string Label::label(bool showOriDblInstead) const
{
	if(!showOriDblInstead || _label != "" || std::isnan(originalValueAsDouble()))
		return _label;
	return ColumnUtils::doubleToString(originalValueAsDouble());
}

std::string Label::labelDisplay() const
{
	return isEmptyValue() ? EmptyValues::displayString() : label();
}

bool Label::isEmptyValue() const
{
	if(!std::isnan(_dblValue) && _column->isEmptyValue(_dblValue))
		return true;
	
	return _column->isEmptyValue(originalValueAsString(false)) || _column->isEmptyValue(label());
}

std::pair<std::string, std::string> Label::origValDisplay() const	
{
	
	const std::string value = originalValueAsString();
	
	return std::make_pair(value, processLabel(label(), value)); 
}

std::string Label::originalValueAsString(bool fancyEmptyValue, bool ignoreEmpty) const
{
	return originalValueAsString(_column, _originalValue, fancyEmptyValue, ignoreEmpty);
}

std::string Label::originalValueAsString(const Column * column, const Json::Value & originalValue, bool fancyEmptyValue, bool ignoreEmpty)
{
	std::string val = _originalValueAsString(column, originalValue, fancyEmptyValue, ignoreEmpty);
	
	if(!column->isEmptyValue(val) || ignoreEmpty)
		return val;
	
	return fancyEmptyValue ? EmptyValues::displayString() : "";
}

std::string Label::_originalValueAsString(const Column * column, const Json::Value & originalValue, bool fancyEmptyValue, bool ignoreEmpty)
{
	
	switch(originalValue.type())
	{
	default:
		return fancyEmptyValue ? EmptyValues::displayString() : "";

	case Json::intValue:
		return std::to_string(originalValue.asInt());

	case Json::realValue:
		return column->doubleToDisplayString(originalValue.asDouble(), fancyEmptyValue, ignoreEmpty);

	case Json::stringValue:
		return originalValue.asString();
	}
}

std::string Label::str() const
{
	return "Label of column '" + _column->name() + "' has display: '" + label() + "' for value " + std::to_string(intsId()) + ", order " + std::to_string(order()) + " and " + ( isEmptyValue() ? "considers itself to be " : "is not ") + "a missing value!";
}

int Label::rowCount(const QModelIndex &parent) const
{
	return parent.isValid() ? 0 : 1;
}

int Label::columnCount(const QModelIndex &parent) const
{
	return parent.isValid() ? 0 : 1;
}

QVariant Label::data(const QModelIndex &index, int role) const
{
	if(!index.isValid())
		return QVariant();
	
	
	if(index.row() >= rowCount() || index.column() >= columnCount())
		return QVariant(); // if there is no data then it doesn't matter what role we play
	
	switch(role)
	{
	case Qt::DisplayRole:									
	case int(dataPkgRoles::label):							return tq(label());
	case int(dataPkgRoles::value):							return tq(originalValueAsString());
	case int(dataPkgRoles::filter):							return filterAllows();
	}
	
	return QVariant();
}



std::string Label::getValue(bool fancyEmptyValue, bool ignoreEmptyValue, bool sepas, columnType asType) const
{
	if(asType == columnType::unknown)
		asType = _column->type();
	
	if (asType == columnType::scale)
		return _column->doubleToDisplayString(originalValueAsDouble(), fancyEmptyValue, ignoreEmptyValue, sepas);

	return originalValueAsString(fancyEmptyValue, ignoreEmptyValue);	
}

std::string Label::getLabel(bool ignoreEmptyValue) const
{
	return ignoreEmptyValue ?	label() 	:	labelDisplay();
}

std::string Label::getDisplay(bool fancyEmptyValue, bool sepas) const
{
	return _column->type() == columnType::scale	
		?	getValue(fancyEmptyValue, false, sepas)
		:	getLabel(false);
}

std::string Label::getShadow(bool fancyEmptyValue, bool sepas) const
{
	return _column->type() != columnType::scale	
		?	getValue(fancyEmptyValue, true, sepas)
		:	getLabel(fancyEmptyValue);
}
