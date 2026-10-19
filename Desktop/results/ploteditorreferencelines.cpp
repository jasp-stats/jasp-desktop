
#include "ploteditorreferencelines.h"
#include "dataenums.h"
#include "qutils.h"
#include "utilities/utiltypes.h"
#include "ploteditormodel.h"

namespace PlotEditor
{

References::References(PlotEditorModel * model)
	: QAbstractTableModel{model}, _model(model)
{

}

int PlotEditor::References::rowCount(const QModelIndex &parent) const
{
	return _refs.size();
}

int PlotEditor::References::columnCount(const QModelIndex &parent) const
{
	return 7;
}

bool References::insertRows(int rows, int count, const QModelIndex &parent)
{
	emit addToUndoStack();
	rows = std::min(rows, int(_refs.size()));
	beginInsertRows(QModelIndex(), rows, rows+count-1);
	for(int c=0; c<count; c++)
		_refs.insert(_refs.begin() + rows + c, Reference());
	endInsertRows();
	emit somethingChanged();
	emit countChanged();
	return true;
}

bool References::removeRows(int rows, int count, const QModelIndex &parent)
{
	rows = std::min(rows, int(_refs.size()) - count);
	if(rows < 0 || count < 1)
		return false;

	emit addToUndoStack();
	beginRemoveRows(QModelIndex(), rows, rows+count-1);
	for(int c=0; c<count; c++)
		_refs.erase(_refs.begin() + rows);
	endRemoveRows();
	emit somethingChanged();
	emit countChanged();
	return true;
}

QVariant PlotEditor::References::data(const QModelIndex &index, int role) const
{
	if(role != Qt::DisplayRole || index.row() < 0 || index.row() >= _refs.size() || index.column() < 0 || index.column() >= columnCount() )
		return QVariant();

	const Reference & ref = _refs[index.row()];

	switch(index.column())
	{
	case 0:			return ref.point ? ReferenceType::Point : ref.horizontal ? ReferenceType::LineHorizontal : ReferenceType::LineVertical;
	case 1:			return ref.text;
	case 2:			return ref.x;
	case 3:			return ref.y;
	case 4:			return ref.color;
	case 5:			return ref.linewidth;
	case 6:			return ref.point ? PointTypeToQString(PointType(ref.linetype)) : LineTypeToQString(LineType(ref.linetype));
	}

	return QVariant();
}

bool References::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if(index.row() < 0 || index.row() > _refs.size() || index.column() < 0 || index.column() >= columnCount() )
		return false;

	if (!_item || !_item->initialized())
		return false;

	if(index.row() == _refs.size())
	{
		if(index.column() == 0 && ReferenceType(value.toInt()) == ReferenceType::Point)
			return false;
		else
			insertRows(index.row(), 1);
	}

	emit addToUndoStack();

	beginResetModel();
	Reference & ref = _refs[index.row()];

	switch(index.column())
	{
	default:
		return false;

	case 0:
	{
		ref.point		= ReferenceType(value.toInt()) == ReferenceType::Point;
		ref.horizontal	= ReferenceType(value.toInt()) == ReferenceType::LineHorizontal;
		break;
	}
	case 1:
	{
		ref.text = value.toString();
		break;
	}
	case 2:
	{
		ref.x = value.toDouble();
		break;
	}
	case 3:
	{
		ref.y = value.toDouble();
		break;
	}
	case 4:
	{
		ref.color = value.toString();
		break;
	}
	case 5:
	{
		ref.linewidth = value.toDouble();
		break;
	}
	case 6:
	{
		std::string str = fq(value.toString());
		if (LineTypeValidName(str))
			ref.linetype = int(LineTypeFromString(str));
		else if (PointTypeValidName(str))
			ref.linetype = int(PointTypeFromString(str));
		break;
	}
	case 7:
		removeRows(index.row(), 1);
		break;
	}
	endResetModel();
	//emit dataChanged(References::index(index.row(), 0), References::index(index.row(), columnCount()));
	emit somethingChanged();
	return true;

}

Json::Value PlotEditor::References::toJson() const
{
	Json::Value val = Json::arrayValue;

	for(const Reference & l : _refs)
	{
		Json::Value obj;
		obj["text"]			= fq(l.text);
		obj["horizontal"]	= l.horizontal;
		obj["point"]		= l.point;
		obj["x"]			= l.x;
		obj["y"]			= l.y;
		obj["color"]		= fq(l.color);
		obj["linewidth"]	= l.linewidth;
		obj["linetype"]		= l.linetype;
		val.append(obj);
	}

	return val;
}

void PlotEditor::References::fromJson(const Json::Value &json)
{
	emit addToUndoStack();
	beginResetModel();
	_refs.clear();
	if(json.type() == Json::arrayValue)
		for(const Json::Value & r : json)
			_refs.push_back(Reference{
				tq(r.get("text",	"???")	.asString()),
				r.get("horizontal", true)	.asBool(),
				r.get("point",		false)	.asBool(),
				r.get("x",			0.)		.asDouble(),
				r.get("y",			0.)		.asDouble(),
				tq(r.get("color",	"black").asString()),
				r.get("linewidth",	1.0)	.asDouble(),
				r.get("linetype",	0)		.asInt()
			});
	endResetModel();
	emit somethingChanged();
	emit countChanged();
}

void References::setItem(QQuickItem *item)
{
	ComponentsListBase* comp = qobject_cast<ComponentsListBase *>(item);
	if (comp)
	{
		_item = comp;
		connect(_item, &ComponentsListBase::addItem,	this,	[this]()			{ insertRows(_refs.size(), 1);	});
		connect(_item, &ComponentsListBase::removeItem, this,	[this](int index)	{ removeRows(index, 1);			});
	}
}

}
