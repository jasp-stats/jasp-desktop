#include "resourcebuttons.h"

ResourceButtons::ResourceButtons(QObject *parent) : QAbstractListModel (parent),
	_data({
		{ButtonType::RecentFiles,	"Recent Files",	false,	"./RecentFiles.qml"	},
		{ButtonType::CurrentFile,	"Current File",	false,	"./CurrentFile.qml"	},
		{ButtonType::Computer,		"Computer",		false,	"./Computer.qml"	},
		{ButtonType::OSF,			"OSF",			true,	"./OSF.qml"			},
		{ButtonType::DataLibrary,	"Data Library",	false,	"./DataLibrary.qml"	}
	})
{
	for(size_t i=0; i<_data.size(); i++)
		_buttonToIndex[_data[i].button] = i;
}
//ameRole = Qt::UserRole + 1, TypeRole, VisibleRole, QmlRole };

QVariant ResourceButtons::data(const QModelIndex &index, int role)	const
{
	if(index.row() < 0 || index.row() > rowCount())
		return QVariant();

	switch(role)
	{
	case Qt::DisplayRole:
	case NameRole:			return _data[size_t(index.row())].name;
	case TypeRole:			return _data[size_t(index.row())].button;
	case VisibleRole:		return _data[size_t(index.row())].visible;
	case QmlRole:			return _data[size_t(index.row())].qml;
	default:				return QVariant();
	}
}


bool ResourceButtons::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if(index.row() < 0 || index.row() > rowCount() || role != VisibleRole)
		return false;

	bool & ref = _data[size_t(index.row())].visible;

	if(ref != value.toBool())
	{
		ref = value.toBool();
		emit dataChanged(index, index, {VisibleRole});
	}

	return true;
}

QHash<int, QByteArray>	ResourceButtons::roleNames() const
{
	static const QHash<int, QByteArray> roles = {
		{ NameRole,		"nameRole"		},
		{ TypeRole,		"typeRole"		},
		{ VisibleRole,	"visibleRole"	},
		{ QmlRole,		"qmlRole"		} };

	return roles;
}

void ResourceButtons::setVisible(ButtonType button, bool visibility)
{
	setData(index(int(_buttonToIndex[button]), 0), visibility, VisibleRole);
}

void ResourceButtons::setOnlyTheseButtonsVisible(std::set<ButtonType> buttons)
{
	for(const DataRow & row : _data)
		setVisible(row.button, buttons.count(row.button) > 0);
}

