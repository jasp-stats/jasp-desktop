#include "ploteditoraxismodel.h"
#include "utilities/qutils.h"
#include "log.h"
#include "gui/messageforwarder.h"

namespace PlotEditor
{

AxisModel::AxisModel(QObject * parent, bool transposed)
	: QAbstractTableModel(parent), _transposed(transposed)
{ }

void AxisModel::setAxisData(const Json::Value & axis)
{
	_axis = axis;
	beginResetModel();

	Json::Value	settings = axis.get(	"settings",		Json::objectValue);

	setTitle(		tq(settings.get(		"title",		""			).asString()));
	setTitleType(	tq(settings.get(		"titleType",	"plainText"	).asString()));
	setType(		tq(settings.get(		"type",			""			).asString()));
	setBreaksType(	tq(settings.get(		"breaksType",	"range"		).asString()));
	setLimitsType(	tq(settings.get(		"limitsType",	"data"		).asString()));

	Json::Value breaks	= settings.get("breaks",	Json::arrayValue);
	Json::Value labels	= settings.get("labels",	Json::arrayValue);
	Json::Value limits	= settings.get("limits",	Json::arrayValue);
	Json::Value expands	= settings.get("expands",	Json::arrayValue);

	fillFromJSON(_breaks, breaks);
	
	if(_breaks.size() > 0)
	{
		_range.clear();
		_range.reserve(3);
		_range.push_back(_breaks[0]);
		_range.push_back(_breaks[_breaks.size() - 1]);
		_range.push_back(_range[0] != _range[1] ? (_range[1] - _range[0]) / (_breaks.size() - 1) : 1);
	}
	
	fillFromJSON(_labels, labels);
	fillFromJSON(_limits, limits);

	endResetModel();
}

Json::Value AxisModel::getAxisData() const
{
	Json::Value		axis	= _axis;

	if(!axis.isMember("settings"))
		axis["settings"] = Json::objectValue;

	Json::Value & settings = axis["settings"];

	settings["title"]	= _title.toStdString();
	settings["type"]	= _type.toStdString();

	if(!settings.get("breaks", Json::nullValue).isArray())
		settings["breaks"] = Json::arrayValue;

	settings["breaks"].clear();

	for(double brk : _breaks)
		settings["breaks"].append(brk);

	if(!settings.get("labels", Json::nullValue).isArray())
		settings["labels"] = Json::arrayValue;

	settings["labels"].clear();

	for(QString label : _labels)
		settings["labels"].append(label.toStdString());

	settings["breaksType"] = _breaksType.toStdString();

	if(!settings.get("range", Json::nullValue).isArray())
		settings["range"] = Json::arrayValue;

	settings["range"].clear();
	for (double v : _range)
		if(std::isnan(v) || std::isinf(v))		settings["range"].append(Json::nullValue);
		else									settings["range"].append(v);

	settings["limitsType"] = _limitsType.toStdString();

	if(!settings.get("limits", Json::nullValue).isArray())
		settings["limits"] = Json::arrayValue;

	settings["limits"].clear();
	for (double v : _limits)
		if(std::isnan(v) || std::isinf(v))		settings["limits"].append(Json::nullValue);
		else									settings["limits"].append(v);

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
	return _transposed ? 2 : int(std::max(_breaks.size(), _labels.size()));
}

int AxisModel::columnCount(const QModelIndex &) const
{
	return _transposed ? int(std::max(_breaks.size(), _labels.size())) : 2;
}

void AxisModel::getEntryAndBreaks(size_t & entry, bool & breaks, const QModelIndex & index) const
{
	entry	= size_t( _transposed ? index.column()	: index.row()	);
	breaks	=		( _transposed ? index.row()		: index.column()) == 0;
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
	if(_transposed == (orientation == Qt::Horizontal) || role != Qt::DisplayRole)
		return QVariant();

	if(section == 0) return "Breaks";
	return "Labels";
}

bool AxisModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if(index.row() < 0 || index.row() >= rowCount() || index.column() < 0 || index.column() >= columnCount())
		return false;

	size_t	entry;
	bool	breaks;
	getEntryAndBreaks(entry, breaks, index);

	if (role == int(specialRoles::insertLeft))
	{
		std::cout << "insertLeft" << std::endl;
		insertBreak(index, entry, true);
		return true;
	}
	else if (role == int(specialRoles::insertRight))
	{
		std::cout << "insertRight" << std::endl;
		insertBreak(index, entry, false);
		return true;
	}
	else if (role == int(specialRoles::deleteBreak))
	{
		std::cout << "deleteBreak" << std::endl;
		deleteBreak(index, entry);
		return true;
	}


	if(breaks)
	{
		double newBreak = value.toDouble();

		if(_breaks[entry] != newBreak)
		{
			bool sizeBigger = QVariant(_breaks[entry]).toString().size() < QVariant(newBreak).toString().size();

			_breaks[entry] = newBreak;

			if(sizeBigger)
			{
				beginResetModel();
				endResetModel();
			}
			else
				emit dataChanged(index, index);

			emit somethingChanged();
		}
		return true;
	}
	else
	{
		QString newLabel = value.toString();

		if(_labels[entry] != newLabel)
		{
			bool sizeBigger = _labels[entry].size() < newLabel.size();

			_labels[entry] = newLabel;

			if(sizeBigger)
			{
				beginResetModel();
				endResetModel();
			}
			else
				emit dataChanged(index, index);

			emit somethingChanged();
		}
		return true;
	}
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

void AxisModel::setTransposed(bool transposed)
{
	if (_transposed == transposed)
		return;

	_transposed = transposed;
	emit transposedChanged(_transposed);
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
