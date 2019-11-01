#include "ploteditoraxismodel.h"
#include "utilities/qutils.h"

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

	setTitle(	tq(	settings.get(		"title",		"").asString()));
	setType(	tq(	settings.get(		"type",			"").asString()));


	Json::Value breaks	= settings.get("breaks",	Json::arrayValue);
	Json::Value labels	= settings.get("labels",	Json::arrayValue);
	Json::Value expands	= settings.get("expands",	Json::arrayValue);

	_breaks.clear();
	_breaks.reserve(breaks.size());
	for(const Json::Value & entry : breaks)
	{
		double v = 0;

		switch(entry.type())
		{
		case Json::intValue:		v = entry.asInt();		break;
		case Json::uintValue:		v = entry.asUInt();		break;
		case Json::realValue:		v = entry.asDouble();	break;
		case Json::booleanValue:	v = entry.asBool();		break;
		}

		_breaks.push_back(v);
	}

	_labels.clear();
	_labels.reserve(labels.size());
	for(const Json::Value & entry : labels)
	{
		std::string str = "";

		switch(entry.type())
		{
		case Json::intValue:		str = std::to_string(entry.asInt());		break;
		case Json::uintValue:		str = std::to_string(entry.asUInt());		break;
		case Json::realValue:		str = std::to_string(entry.asDouble());		break;
		case Json::stringValue:		str = entry.asString();						break;
		case Json::booleanValue:	str = entry.asBool() ? "True" : "False";	break;
		}

		_labels.push_back(tq(str));
	}

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

}
