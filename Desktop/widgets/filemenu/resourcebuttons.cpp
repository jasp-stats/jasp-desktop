#include "resourcebuttons.h"

ResourceButtons::ResourceButtons(QObject *parent) : QAbstractListModel (parent),
	_data()
{
	loadButtonData(_data);

	for(size_t i=0; i<_data.size(); i++)
		_buttonToIndex[_data[i].button] = i;
}

void ResourceButtons::loadButtonData(std::vector<ResourceButtons::DataRow> &data)
{
	_data = {
		{ButtonType::RecentFiles,	tr("Recent Files"),	false,	"./RecentFiles.qml"		, true},
		{ButtonType::CurrentFile,	tr("Current File"),	false,	"./CurrentFile.qml"		, false},
		{ButtonType::Computer,		tr("Computer"),		false,	"./Computer.qml"		, true},
		{ButtonType::OSF,			tr("OSF"),			false,	"./OSF.qml"				, true},
		{ButtonType::DataLibrary,	tr("Data Library"),	false,	"./DataLibrary.qml"		, true},
		{ButtonType::PrefsData,		tr("Data"),			false,	"./PrefsData.qml"		, true},
		{ButtonType::PrefsResults,	tr("Results"),		false,	"./PrefsResults.qml"	, true},
		{ButtonType::PrefsUI,		tr("Interface"),	false,	"./PrefsUI.qml"			, true},
		{ButtonType::PrefsAdvanced,	tr("Advanced"),		false,	"./PrefsAdvanced.qml"	, true}
	};

}

QVariant ResourceButtons::data(const QModelIndex &index, int role)	const
{
	if(index.row() < 0 || index.row() > rowCount())
		return QVariant();

	size_t row = size_t(index.row());

	switch(role)
	{
	case Qt::DisplayRole:
	case NameRole:			return _data[row].name;
	case TypeRole:			return _data[row].button;
	case VisibleRole:		return _data[row].visible;
	case QmlRole:			return _data[row].qml;
	case EnabledRole:		return _data[row].enabled;
	case SelectedRole:		return _data[row].button == _selectedButton;

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
		{ EnabledRole,	"enabledRole"	},
		{ SelectedRole,	"selectedRole"	} };

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

		if(row.button == selectedButton() && !row.visible)
			setSelectedButton(None);
	}
}

void ResourceButtons::setButtonEnabled(ResourceButtons::ButtonType button, bool enabled)
{
	size_t buttonIndex			= _buttonToIndex[button];
	QModelIndex modelIndex		= index(int(buttonIndex), 0);
	_data[buttonIndex].enabled	= enabled;

	emit dataChanged(modelIndex, modelIndex, {EnabledRole});
}


void ResourceButtons::setCurrentQML(QString currentQML)
{
	if (_currentQML == currentQML)
		return;

	_currentQML = currentQML;
	emit currentQMLChanged(_currentQML);
}

void ResourceButtons::setSelectedButton(ButtonType selectedButton)
{
	if (_selectedButton == selectedButton)
		return;

	ButtonType oldButton	= _selectedButton;
	_selectedButton			= selectedButton;

	if(oldButton != None)
	{
		QModelIndex	oldIndex = index(_buttonToIndex[oldButton]);
		emit dataChanged(oldIndex, oldIndex);
	}

	if(_selectedButton != None)
	{
		QModelIndex	newIndex = index(_buttonToIndex[_selectedButton]);
		emit dataChanged(newIndex, newIndex);
		setCurrentQML(qml(_selectedButton));
	}
	else
		setCurrentQML("");

	emit selectedButtonChanged(_selectedButton);
}


void ResourceButtons::selectFirstButtonIfNoneSelected()
{
	if(_selectedButton == None)
		for(DataRow & rij : _data)
			if(rij.visible && rij.enabled)
			{
				setSelectedButton(rij.button);
				return;
			}
}

void ResourceButtons::selectButtonUp()
{
	int idx = _buttonToIndex[_selectedButton];

	for(int move = 1; move < _data.size(); move++)
	{
		size_t idxUp = ((_data.size() + idx) - move) % _data.size();

		if(_data[idxUp].visible && _data[idxUp].enabled)
		{
			setSelectedButton(_data[idxUp].button);
			return;
		}
	}
}

void ResourceButtons::selectButtonDown()
{
	int idx = _buttonToIndex[_selectedButton];

	for(int move = 1; move < _data.size(); move++)
	{
		size_t idxDown = (idx + move) % _data.size();

		if(_data[idxDown].visible && _data[idxDown].enabled)
		{
			setSelectedButton(_data[idxDown].button);
			return;
		}
	}
}

void ResourceButtons::refresh()
{
	beginResetModel();

	std::vector<DataRow> savedata = _data;

	loadButtonData(_data);

	for(int i=0 ; i < savedata.size() ; i++)
	{
		_data[i].enabled = savedata[i].enabled;
		_data[i].visible = savedata[i].visible;
	}

	endResetModel();

}
