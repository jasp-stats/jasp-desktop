#include "ploteditoraxismodel.h"
#include "utilities/qutils.h"
#include "utilities/jsonutilities.h"
#include "log.h"
#include "gui/messageforwarder.h"

namespace PlotEditor
{

void AxisModel::setAxisData(const Json::Value & axis)
{
	_axis = axis;
	beginResetModel();

	_axisType = tq(axis.get("type", "").asString());
//	this should never happen
//	if (_axisType == "")
//		throw error

	Json::Value	settings = axis.get(	"settings",		Json::objectValue);

	setTitle(		tq(settings.get(	"title",		""			).asString()));
	setTitleType(	tq(settings.get(	"titleType",	"plainText"	).asString()));
	setType(		tq(settings.get(	"type",			""			).asString()));
	setBreaksType(	tq(settings.get(	"breaksType",	"range"		).asString()));
	setLimitsType(	tq(settings.get(	"limitsType",	"data"		).asString()));

	Json::Value breaks	= settings.get(	"breaks",		Json::arrayValue);
	Json::Value labels	= settings.get(	"labels",		Json::arrayValue);
	Json::Value limits	= settings.get(	"limits",		Json::arrayValue);
	Json::Value expands	= settings.get(	"expands",		Json::arrayValue);

	fillFromJSON(_breaks, breaks);
	
	if(hasBreaks())
	{
		_range.clear();
		_range.reserve(3);
		_range.push_back(_breaks[0]);
		_range.push_back(_breaks[_breaks.size() - 1]);
		_range.push_back(_range[0] != _range[1] ? (_range[1] - _range[0]) / (_breaks.size() - 1) : 1);
	}
	else
		_range.clear();
	
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
	settings["type"]		= _type.toStdString();
	settings["breaks"]		= JsonUtilities::vecToJsonArray(_breaks);
	settings["labels"]		= JsonUtilities::vecToJsonArray(_labels);
	settings["range"]		= JsonUtilities::vecToJsonArray(_range);
	settings["limits"]		= JsonUtilities::vecToJsonArray(_limits);
	settings["breaksType"]	= _breaksType.toStdString();
	settings["limitsType"]	= _limitsType.toStdString();

	return axis;
}

void AxisModel::setTitle(QString title)
{
	if (_title == title)
		return;

	_title = title;
	emit titleChanged(_title);
	emit somethingChanged();
}

void AxisModel::setTitleType(QString titleType)
{
	if (_titleType == titleType)
		return;

	_titleType = titleType;
	emit titleTypeChanged(_titleType);
	emit somethingChanged();
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

bool AxisModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if(index.row() < 0 || index.row() >= rowCount() || index.column() < 0 || index.column() >= columnCount())
		return false;

	size_t	entry;
	bool	breaks;
	getEntryAndBreaks(entry, breaks, index);

	switch(role)
	{
	case int(specialRoles::insertLeft):
	{
		Log::log() << "insertLeft" << std::endl;
		insertBreak(index, entry, true);
		return true;
	}

	case int(specialRoles::insertRight):
	{
		Log::log() << "insertRight" << std::endl;
		insertBreak(index, entry, false);
		return true;
	}

	case int(specialRoles::deleteBreak):
	{
		Log::log() << "deleteBreak" << std::endl;
		deleteBreak(index, entry);
		return true;
	}

	default:
		break;
	}

	if(breaks)
	{
		double newBreak = value.toDouble();

		if(_breaks[entry] != newBreak)
		{
			_breaks[entry] = newBreak;
			emit dataChanged(index, index);
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

	_type = type;
	emit typeChanged(_type);
	emit somethingChanged();
}

void AxisModel::setVertical(bool vertical)
{
	if (_vertical == vertical)
		return;

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
	emit dataChanged(index, index);
	emit somethingChanged();

}

void AxisModel::deleteBreak(const QModelIndex &index, const size_t column)
{
	Log::log()	<<	"AxisModel::deleteBreak() column index is: " << column  << std::endl;

	beginRemoveColumns(QModelIndex(), column, column + 1);

	_breaks.erase(_breaks.begin() + column);
	_labels.erase(_labels.begin() + column);

	endRemoveColumns();
	emit dataChanged(index, index);
	emit somethingChanged();
}

void AxisModel::setBreaksType(const QString breaksType)
{
	if (_breaksType == breaksType)
		return;
	_breaksType = breaksType;
	emit rangeChanged();
	emit somethingChanged();
}

void AxisModel::setRange(const double value, const size_t idx)
{
	if(_range.size() <= idx)			_range.resize(idx + 1);
	else if(_range[idx] == value)		return;
	
	_range[idx] = value;
	emit rangeChanged();
	emit somethingChanged();
}

void AxisModel::setLimitsType(const QString limitsType)
{
	if (_limitsType == limitsType)
		return;
	
	_limitsType = limitsType;
	emit limitsChanged();
	emit somethingChanged();
}

void AxisModel::setLimits(const double value, const size_t idx)
{
	if(_limits.size() <= idx)			_limits.resize(idx + 1);
	else if (_limits[idx] == value)		return;
	
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

QHash<int, QByteArray> AxisModel::roleNames() const
{
	static bool						set = false;
	static QHash<int, QByteArray> roles = QAbstractItemModel::roleNames ();

	if(!set)
	{
		roles[int(specialRoles::insertLeft)]							= QString("insertLeft").toUtf8();
		roles[int(specialRoles::insertRight)]							= QString("insertRight").toUtf8();
		roles[int(specialRoles::deleteBreak)]							= QString("deleteBreak").toUtf8();

		set = true;
	}

	return roles;
}

}
