#include "ploteditoraxismodel.h"
#include "ploteditormodel.h"
#include "qutils.h"
#include "jsonutilities.h"
#include "log.h"
#include "messageforwarder.h"
#include "utils.h"

namespace PlotEditor
{

void AxisModel::setAxisData(const Json::Value & axis)
{
	_axis = axis;
	beginResetModel();

	_axisType	= tq(axis.get("type", "").asString());
	_continuous = _axisType != "ScaleDiscrete";

	Json::Value	settings = axis.get(	"settings",		Json::objectValue);

	setTitle(		tq(						settings.get(	"title",		""	).asString()));
	setTitleType(	TitleTypeFromString(	settings.get(	"titleType",	""	).asString())); //Defaults are set in *TypeFromString
	setType(		tq(						settings.get(	"type",			""	).asString()));
	setBreaksType(	BreaksTypeFromString(	settings.get(	"breaksType",	""	).asString()));
	setLimitsType(	LimitsTypeFromString(	settings.get(	"limitsType",	""	).asString()));

	Json::Value breaks	= settings.get(	"breaks",		Json::arrayValue);
	Json::Value labels	= settings.get(	"labels",		Json::arrayValue);
	Json::Value limits	= settings.get(	"limits",		Json::arrayValue);
	Json::Value expands	= settings.get(	"expands",		Json::arrayValue);
	Json::Value range	= settings.get(	"range",		Json::arrayValue);

	fillFromJSON(_breaks, breaks);
	fillFromJSON(_range, range);

//	code below may be useful if we wish to update the breaks in C++ upon adjusting the range
//	if(hasBreaks())
//	{
//		_range.clear();
//		_range.reserve(3);
//		_range.push_back()
//		_range.push_back(_breaks[0]);
//		_range.push_back(_breaks[_breaks.size() - 1]);
//		_range.push_back(_range[0] != _range[1] ? (_range[1] - _range[0]) / (_breaks.size() - 1) : 1);
//	}
//	else
//		_range.clear();

	fillFromJSON(_labels, labels);
	fillFromJSON(_limits, limits);

	endResetModel();
}

Json::Value AxisModel::getAxisData() const
{
	Json::Value		axis	= _axis;

	if(!axis.isMember("settings"))
		axis["settings"] = Json::objectValue;

	Json::Value & settings	= axis["settings"];

	settings["title"]		= _title.toStdString();
	settings["titleType"]	= TitleTypeToString(_titleType);
	settings["type"]		= _type.toStdString();
	settings["breaks"]		= JsonUtilities::vecToJsonArray(_breaks);
	settings["labels"]		= JsonUtilities::vecToJsonArray(_labels);
	settings["range"]		= JsonUtilities::vecToJsonArray(_range);
	settings["limits"]		= JsonUtilities::vecToJsonArray(_limits);
	settings["breaksType"]	= BreaksTypeToString(_breaksType);
	settings["limitsType"]	= LimitsTypeToString(_limitsType);

	return axis;
}

void AxisModel::setTitle(QString title)
{
	if (_title == title)
		return;

	emit addToUndoStack();

	_title = title;
	emit titleChanged(_title);
	emit somethingChanged();
}

void AxisModel::setTitleType(TitleType titleType)
{
	if (_titleType == titleType)
		return;

	emit addToUndoStack();

	_titleType = titleType;
	emit titleTypeChanged(_titleType);
	emit somethingChanged();
}

AxisModel::AxisModel(PlotEditorModel *parent, bool vertical) : QAbstractTableModel(parent), _plotEditor(parent), _vertical(vertical)
{

}

int AxisModel::rowCount(const QModelIndex &) const
{
	return _vertical ? (hasBreaks() ? 2 : 1) : int(std::max(_breaks.size(), _labels.size()));
}

int AxisModel::columnCount(const QModelIndex &) const
{
	return _vertical ? int(std::max(_breaks.size(), _labels.size())) : (hasBreaks() ? 2 : 1);
}

void AxisModel::getEntryAndBreaks(size_t & entry, bool & breaks, const QModelIndex & index) const
{
	entry	= size_t(			_vertical ? index.column()	: index.row()	);
	breaks	= hasBreaks() && (	_vertical ? index.row()		: index.column()) == 0;
}

void AxisModel::refresh()
{
	beginResetModel();
	endResetModel();
	emit titleTypeChanged(titleType());
	emit typeChanged(type());
	emit rangeChanged();
	emit limitsChanged();
}

QVariant AxisModel::data(const QModelIndex &index, int role) const
{
	if(role != Qt::DisplayRole || index.row() < 0 || index.row() >= rowCount() || index.column() < 0 || index.column() >= columnCount())
		return QVariant();

	size_t	entry;
	bool	breaks;
	getEntryAndBreaks(entry, breaks, index);

	if(breaks)	return _breaks.size() > entry ? _breaks[entry] : QVariant();
	else		return _labels.size() > entry ? _labels[entry] : QVariant();

}

QVariant AxisModel::headerData ( int section, Qt::Orientation orientation, int role) const
{
	if(_vertical == (orientation == Qt::Horizontal) || role != Qt::DisplayRole)
		return QVariant();

	if(section == 0 && hasBreaks()) return "Breaks";
	else							return "Labels";
}

bool AxisModel::insertColumns(int column, int count, const QModelIndex &)
{
	for (int i = 0; i < count; i++)
	{
		bool left = true;

		if (column < 0 || columnCount() == 0)	column = 0;
		else if (column >= columnCount())
		{
			column = columnCount() - 1;
			left = false;
		}
		insertBreak(index(column, column), size_t(column), left);
	}

	return true;
}

bool AxisModel::removeColumns(int column, int count, const QModelIndex &)
{
	if (column < 0 || column >= columnCount()) return false;

	for (int i = 0; i < count; i++)
		deleteBreak(index(column, column), size_t(column));

	return true;
}

bool AxisModel::setData(const QModelIndex &index, const QVariant &value, int)
{
	if(index.row() < 0 || index.row() >= rowCount() || index.column() < 0 || index.column() >= columnCount())
		return false;

	size_t	entry;
	bool	breaks;
	getEntryAndBreaks(entry, breaks, index);

	if(breaks)
	{
		double newBreak = value.toDouble();

		if(!Utils::isEqual(_breaks[entry], newBreak))
		{
			double oldBreak = _breaks[entry];

			if ((entry > 0 && _breaks[entry-1] >= newBreak)
				|| (entry < _breaks.size() - 1 && _breaks[entry+1] <= newBreak))
			{
				// A break value should not be smaller that its left one, and higher than its reight one.
				// Force the view to change its value back to the old value.
				// This model is the source of a TableView, and if the _breaks[entry] is not changed,
				// the TableView model will not see any difference in its values and will not ask the TableView to be updated.
				_breaks[entry] = newBreak;
				emit dataChanged(index, index);
				_breaks[entry] = oldBreak;
				emit dataChanged(index, index);
				return false;
			}

			QString label = _labels[entry];
			bool labelIsDouble = false;
			double labelDouble = label.toDouble(&labelIsDouble);
			QModelIndex index2 = index;

			if (labelIsDouble && (Utils::isEqual(labelDouble, oldBreak)))
			{
				// If the label is the same as the break value, then change olso the label.
				_labels[entry] = value.toString();
				int col = _vertical ? int(entry) : 1;
				int row = _vertical ? 1 : int(entry);
				index2 = AxisModel::index(row, col);
			}

			_breaks[entry] = newBreak;

			if (entry == _breaks.size() - 1)	setTo(newBreak);
			if (entry == 0)						setFrom(newBreak);

			emit dataChanged(index, index2);
			emit somethingChanged();
		}
	}
	else
	{
		QString newLabel = value.toString();

		if(_labels[entry] != newLabel)
		{
			_labels[entry] = newLabel;
			emit dataChanged(index, index);
			emit somethingChanged();
		}
	}

	return true;
}

Qt::ItemFlags AxisModel::flags(const QModelIndex &) const
{
	return Qt::ItemIsEnabled | Qt::ItemIsEditable;
}

void AxisModel::setType(QString type)
{
	if (_type == type)
		return;

	emit addToUndoStack();
	_type = type;
	emit typeChanged(_type);
	emit somethingChanged();
}

void AxisModel::setVertical(bool vertical)
{
	if (_vertical == vertical)
		return;

	emit addToUndoStack();
	_vertical = vertical;
	emit verticalChanged(_vertical);
}

void AxisModel::insertBreak(const QModelIndex &index, const size_t column, const bool left)
{
	Log::log()	<<	"AxisModel::insertBreak() column index is: " << column << "left is: " << left << std::endl;
	
	if(_breaks.size() < 2) 
	{
		MessageForwarder::showWarning("Fix AxisModel::insertBreak", "Inserting a break should be possible but  AxisModel::insertBreak only covers situations where there are already breaks.\n\nPlease fix it!");
		return;
	}

	// TODO: this crashes when there are less than x breaks
	const size_t size		= _breaks.size() - 1; //Note to whoever wrote the previous version: _breaks.size() is unsigned, so if you do (size_t(0) - 1) you 2^64 - 1... While that is probably not what you wanted. But this code should be fixed anyway 
	const size_t position	= static_cast<size_t>(!left);

	double insertedValue;
	
	// Logic below checks if we're inserting at an extreme, if so extrapolate with the previous step, otherwise interpolate between nearest breaks
	if (column == 0 && left)			insertedValue = _breaks[0] + _breaks[0] - _breaks[1];
	else if (column == size && !left)	insertedValue = _breaks[size] + _breaks[size] - _breaks[size - 1];
	else								insertedValue = (_breaks[column - 1 + position] + _breaks[column + position]) / 2.0;

	// Why QModelIndex() rather than index??
	beginInsertColumns(QModelIndex(), column + position, column + position + 1);

	_breaks.insert(_breaks.begin() + column + position, insertedValue);
	// TODO: use the precision of the present labels?
	_labels.insert(_labels.begin() + column + position, QString::number(insertedValue));//, 'f', 3))

	endInsertColumns();

	if (insertedValue > to())	setTo(insertedValue);
	if (insertedValue < from())	setFrom(insertedValue);

	emit dataChanged(index, index);
	emit somethingChanged();
	emit hasBreaksChanged();
}

void AxisModel::deleteBreak(const QModelIndex &index, const size_t column)
{
	Log::log()	<<	"AxisModel::deleteBreak() column index is: " << column  << std::endl;
	if (_breaks.size() <= 2) return;

	emit addToUndoStack();

	beginRemoveColumns(QModelIndex(), column, column + 1);

	_breaks.erase(_breaks.begin() + column);
	_labels.erase(_labels.begin() + column);

	endRemoveColumns();

	if (column == _breaks.size())	setTo(_breaks[column - 1]);
	if (column == 0)				setFrom(_breaks[0]);

	emit dataChanged(index, index);
	emit somethingChanged();
	emit hasBreaksChanged();
}

void AxisModel::setBreaksType(const BreaksType breaksType)
{
	if (_breaksType == breaksType)
		return;

	emit addToUndoStack();
	_breaksType = breaksType;
	emit rangeChanged();
	emit somethingChanged();
	emit hasBreaksChanged(); // Maybe?
}

void AxisModel::setRange(const double value, const size_t idx)
{

	if(_range.size() <= idx)
	{
		Log::log() << "Impossible index passed to setRange: idx: " << idx << " | value: " << value << std::endl;
		return;
	}
	else if(Utils::isEqual(_range[idx], value))		return;

	emit addToUndoStack();
	_range[idx] = value;

	emit rangeChanged();
	emit somethingChanged();
}

void AxisModel::setFrom(const double newFrom)
{
	double oldFrom = from();
	setRange(newFrom, 0);

	if (Utils::isEqual(lower(), oldFrom))	setLimits(newFrom, 0);
}

void AxisModel::setTo(const double newTo)
{
	double oldTo = to();
	setRange(newTo, 1);

	if (Utils::isEqual(upper(), oldTo))		setLimits(newTo, 1);
}

void AxisModel::setLimitsType(const LimitsType limitsType)
{
	if (_limitsType == limitsType)
		return;

	emit addToUndoStack();

	_limitsType = limitsType;
	emit limitsChanged();

	emit somethingChanged();
}

void AxisModel::setLimits(const double value, const size_t idx)
{
	if(_limits.size() <= idx)
	{
		Log::log() << "Impossible index passed to setLimits: idx: " << idx << " | value: " << value << std::endl;
		return;
	}
	else if (Utils::isEqual(_limits[idx], value))		return;

	emit addToUndoStack();

	_limits[idx] = value;
	emit limitsChanged();

	emit somethingChanged();
}

void AxisModel::fillFromJSON(std::vector<double> &obj, Json::Value value)
{
	obj.clear();
	obj.reserve(value.size());
	for(const Json::Value & entry : value)
	{
		double v;
		
		switch(entry.type())
		{
		case Json::intValue:		v = entry.asInt();		break;
		case Json::uintValue:		v = entry.asUInt();		break;
		case Json::realValue:		v = entry.asDouble();	break;
		case Json::booleanValue:	v = entry.asBool();		break;
		default:					v = 0;					break;
		}

		obj.push_back(v);
	}
}

void AxisModel::fillFromJSON(std::vector<QString> &obj, Json::Value value)
{
	obj.clear();
	obj.reserve(value.size());
	for(const Json::Value & entry : value)
	{
		std::string str;

		switch(entry.type())
		{
		case Json::intValue:		str = std::to_string(entry.asInt());		break;
		case Json::uintValue:		str = std::to_string(entry.asUInt());		break;
		case Json::realValue:		str = std::to_string(entry.asDouble());		break;
		case Json::stringValue:		str = entry.asString();						break;
		case Json::booleanValue:	str = entry.asBool() ? "True" : "False";	break;
		default:					str = "";									break;
		}
		obj.push_back(tq(str));
	}
}


std::string		AxisModel::TitleTypeToString (TitleType  type) const
{
	switch(type)
	{
	case TitleType::TitleCharacter:		return "character";
	case TitleType::TitleExpression:	return "expression";
	case TitleType::TitleLaTeX:			return "LaTeX";
	case TitleType::TitleNull:			return "NULL";
	}

	return "???"; //Can't get here but GCC doesnt understand that
}

std::string		AxisModel::BreaksTypeToString(BreaksType type) const
{
	switch(type)
	{
	case BreaksType::BreaksManual:		return "manual";
	case BreaksType::BreaksRange:		return "range";
	case BreaksType::BreaksNull:		return "NULL";
	}

	return "???"; //Can't get here but GCC doesnt understand that
}

std::string		AxisModel::LimitsTypeToString(LimitsType type) const
{
	switch(type)
	{
	case LimitsType::LimitsBreaks:		return "breaks";
	case LimitsType::LimitsData:		return "data";
	case LimitsType::LimitsManual:		return "manual";
	}

	return "???"; //Can't get here but GCC doesnt understand that
}

AxisModel::TitleType	AxisModel::TitleTypeFromString (const std::string& type) const
{
	if(type == "expression")	return TitleType::TitleExpression;
	if(type == "LaTeX")			return TitleType::TitleLaTeX;
	if(type == "NULL")			return TitleType::TitleNull;
								return TitleType::TitleCharacter; //default
}

AxisModel::BreaksType	AxisModel::BreaksTypeFromString(const std::string& type) const
{
	if(type == "manual"	)	return BreaksType::BreaksManual;
	if(type == "NULL")		return BreaksType::BreaksNull;
							return BreaksType::BreaksRange; //default
}

AxisModel::LimitsType	AxisModel::LimitsTypeFromString(const std::string& type) const
{
	if(type == "breaks"	)	return LimitsType::LimitsBreaks;
	if(type == "manual"	)	return LimitsType::LimitsManual;
							return LimitsType::LimitsData; //default
}


}
