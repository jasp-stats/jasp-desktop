#include "resourcebuttons.h"

ResourceButtons::ResourceButtons(QObject *parent) : QAbstractListModel (parent),
	_data({
		{ButtonType::RecentFiles,	"Recent Files",	false,	"./RecentFiles.qml"		, true},
		{ButtonType::CurrentFile,	"Current File",	false,	"./CurrentFile.qml"		, false},
		{ButtonType::Computer,		"Computer",		false,	"./Computer.qml"		, true},
		{ButtonType::OSF,			"OSF",			false,	"./OSF.qml"				, true},
		{ButtonType::DataLibrary,	"Data Library",	false,	"./DataLibrary.qml"		, true},
		{ButtonType::PrefsData,		"Data",			false,	"./PrefsData.qml"		, true},
		{ButtonType::PrefsResults,	"Results",		false,	"./PrefsResults.qml"	, true},
		{ButtonType::PrefsAdvanced,	"Advanced",		false,	"./PrefsAdvanced.qml"	, true}
	})
{
	for(size_t i=0; i<_data.size(); i++)
		_buttonToIndex[_data[i].button] = i;

	connect(this, &ResourceButtons::clicked, this, &ResourceButtons::clickedHandler);
}
//nameRole = Qt::UserRole + 1, TypeRole, VisibleRole, QmlRole };

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
	case EnabledRole:		return _data[size_t(index.row())].enabled;

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
		{ QmlRole,		"qmlRole"		},
		{ EnabledRole,	"enabledRole"	}};

	return roles;
}

void ResourceButtons::setVisible(ButtonType button, bool visibility)
{
	setData(index(int(_buttonToIndex[button]), 0), visibility, VisibleRole);
}

QString ResourceButtons::qml(ResourceButtons::ButtonType button)
{
	size_t index = _buttonToIndex[button];
	if(index < 0 || index >= _data.size())
		return "";

	return _data[index].qml;
}

void ResourceButtons::setOnlyTheseButtonsVisible(std::set<ButtonType> buttons)
{
	for(const DataRow & row : _data)
	{
		setVisible(row.button, buttons.count(row.button) > 0);
		if(row.qml == currentQML() && !row.visible)
			setCurrentQML("");
	}
}

void ResourceButtons::setButtonEnabled(ResourceButtons::ButtonType button, bool enabled)
{
	size_t buttonIndex = _buttonToIndex[button];
	QModelIndex modelIndex = index(int(buttonIndex), 0);
	_data[buttonIndex].enabled = enabled;

	emit dataChanged(modelIndex, modelIndex, {EnabledRole});
}


void ResourceButtons::setCurrentQML(QString currentQML)
{
	if (_currentQML == currentQML)
		return;

	_currentQML = currentQML;
	emit currentQMLChanged(_currentQML);
}
