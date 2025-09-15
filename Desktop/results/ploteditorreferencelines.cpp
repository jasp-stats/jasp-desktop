#include "ploteditorreferencelines.h"
#include "data/datasetpackageenums.h"
#include "utilities/qutils.h"
#include "ploteditormodel.h"

namespace PlotEditor
{

References::References(PlotEditorModel * model)
	: QAbstractTableModel{model}, _model(model)
{
	
}

int PlotEditor::References::rowCount(const QModelIndex &parent) const
{
	return _refs.size() + 1;
}

int PlotEditor::References::columnCount(const QModelIndex &parent) const
{
	return 5;
}

// I hate the following and fixes for this kind of shenanigans are in a different branch but for now just do this:
QHash<int, QByteArray> PlotEditor::References::roleNames() const
{
	static bool						set = false;
	static QHash<int, QByteArray> roles = QAbstractItemModel::roleNames ();

	if(!set)
	{
		for(const auto & enumString : dataPkgRolesToStringMap())
			roles[int(enumString.first)] = tq(enumString.second).toUtf8();

		set = true;
	}

	return roles;
}

bool References::indexDisabled(const QModelIndex &index) const
{
	if(index.row() >= rowCount())
		return false;
	
	if(index.row() >= _refs.size() || _refs[index.row()].point)
		return false;
	
	if(_refs[index.row()].horizontal && index.column() == 2)
		return true;
	
	if(!_refs[index.row()].horizontal && index.column() == 3)
		return true;
	
	return false;
}

Qt::ItemFlags References::flags(const QModelIndex &index) const
{
	Qt::ItemFlags flags = QAbstractTableModel::flags(index) | Qt::ItemIsEditable;
	if(indexDisabled(index))
		flags &= ~Qt::ItemIsEnabled;
	return flags;
}

QVariant PlotEditor::References::headerData(int section, Qt::Orientation orientation, int role) const
{
	if(orientation == Qt::Vertical)
		return QVariant();
	
	switch(role)
	{
	default:
		switch(section)
		{
		case 0:			return tr("Type");
		case 1:			return tr("Text");
		case 2:			return tr("Horizontal");
		case 3:			return tr("Vertical");
		case 4:			return tr("Remove");
		};
		break;
	
	case int(dataPkgRoles::maxColString):
	case int(dataPkgRoles::maxRowHeaderString):
	case int(dataPkgRoles::maxColumnHeaderString):
		return QVariant();
		
	case int(dataPkgRoles::columnWidthFallback):
		return _widths.size() > section ? _widths[section] : 200;
	}
	
	return QVariant();
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
	return true;
}

QVariant PlotEditor::References::data(const QModelIndex &index, int role) const
{
	if(role != Qt::DisplayRole || index.row() < 0 || index.row() > _refs.size() || index.column() < 0 || index.column() >= columnCount() )
		return QVariant();
	
	if(index.row() == _refs.size()) //Special "add a row"-row
		switch(index.column())
		{
		case 0:			return ReferenceType::Point;
		default:		return "";
		case 2:			return 0;
		case 3:			return 0;
		}
	
	const Reference & ref = _refs[index.row()];
	
	switch(index.column())
	{
	case 0:			return ref.point ? ReferenceType::Point : ref.horizontal ? ReferenceType::LineHorizontal : ReferenceType::LineVertical;
	case 1:			return ref.text;
	case 2:			return ref.x;
	case 3:			return ref.y;
	}
									   
	return QVariant();
}
									   
bool References::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if(index.row() < 0 || index.row() > _refs.size() || index.column() < 0 || index.column() >= columnCount() )
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
				r.get("y",			0.)		.asDouble()
			});
	endResetModel();
	emit somethingChanged();
}

void References::setColWidth(int index, int width)
{
	if(_widths.size() > index && _widths[index] == width)
		return;
	
	if(_widths.size() <= index)
		_widths.resize(index+1);
	_widths[index] = width;
}

int References::viewWidth() const
{
	return _viewWidth;
}

void References::setViewWidth(int newViewWidth)
{
	if (_viewWidth == newViewWidth)
		return;
	_viewWidth = newViewWidth;
	emit viewWidthChanged();
	
	int fiver = newViewWidth / 5;
	
	beginResetModel();
	for(int i=0; i<5; i++)
		setColWidth(i, fiver);
	endResetModel();
}

}
