#include "actionbuttons.h"
#include <QObject>
#include <QtQml>
#include "log.h"

ActionButtons::ActionButtons(QObject *parent) : QAbstractListModel (parent),
  _data()

{
	loadButtonData(_data);

	for(size_t i=0; i<_data.size(); i++)
		_opToIndex[_data[i].operation] = i;

	qmlRegisterUncreatableType<ActionButtons>("FileOperation",1,0,"FileOperation","Impossible create a FileOperation Object");

	connect(this, &ActionButtons::buttonClicked, this, &ActionButtons::setSelectedAction);
}

void ActionButtons::loadButtonData(std::vector<ActionButtons::DataRow> &data)
{
	_data =
	{
		{FileOperation::Open,			tr("Open"),				true,	{ResourceButtons::RecentFiles,	ResourceButtons::Computer,	ResourceButtons::DataLibrary, ResourceButtons::OSF }		},
		{FileOperation::Save,			tr("Save"),				false,	{}																														},
		{FileOperation::SaveAs,			tr("Save As"),			false,	{ResourceButtons::Computer, ResourceButtons::OSF }																		},
		{FileOperation::ExportResults,	tr("Export Results"),	false,	{ResourceButtons::Computer, ResourceButtons::OSF }																		},
		{FileOperation::ExportData,		tr("Export Data"),		false,	{ResourceButtons::Computer, ResourceButtons::OSF }																		},
		{FileOperation::SyncData,		tr("Sync Data"),		false,	{ResourceButtons::CurrentFile, ResourceButtons::Computer, ResourceButtons::OSF }										},
		{FileOperation::Close,			tr("Close"),			false,	{}																														},
		{FileOperation::Preferences,	tr("Preferences"),		true,	{ResourceButtons::PrefsData, ResourceButtons::PrefsResults, ResourceButtons::PrefsUI, ResourceButtons::PrefsAdvanced}	},
		{FileOperation::About,			tr("About"),			true,	{}																														},
		{FileOperation::RCmd,			tr("R (Beta)"),			true,	{}																														}
	  };
}

QVariant ActionButtons::data(const QModelIndex &index, int role)	const
{
	if(index.row() < 0 || index.row() > rowCount())
		return QVariant();

	size_t rij = size_t(index.row());

	switch(role)
	{
	case Qt::DisplayRole:
	case NameRole:				return _data[rij].name;
	case TypeRole:				return _data[rij].operation;
	case EnabledRole:			return _data[rij].enabled;
	case SelectedRole:			return _data[rij].operation == _selected;
	case ResourceButtonsRole:	return _data[rij].resourceButtons.size() > 0;
	default:					throw std::runtime_error("Unknown role for actionButtons!");
	}
}


bool ActionButtons::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if(index.row() < 0 || index.row() > rowCount() || role != EnabledRole)
		return false;

	bool & ref = _data[size_t(index.row())].enabled;

	if(ref != value.toBool())
	{
		ref = value.toBool();
		emit dataChanged(index, index, {EnabledRole});
	}

	return true;
}

QHash<int, QByteArray>	ActionButtons::roleNames() const
{
	static const QHash<int, QByteArray> roles = {
		{ NameRole,				"nameRole"			},
		{ TypeRole,				"typeRole"			},
		{ EnabledRole,			"enabledRole"		},
		{ SelectedRole,			"selectedRole"		},
		{ ResourceButtonsRole,	"hasSubMenuRole"	}};

	return roles;
}

void ActionButtons::setEnabled(FileOperation operation, bool enabledState)
{
	setData(index(int(_opToIndex[operation]), 0), enabledState, EnabledRole);
}

void ActionButtons::setSelectedAction(FileOperation selectedAction)
{
	if (_selected == selectedAction)
		return;

	bool		oldIsNone = _selected		== FileOperation::None,
				newIsNone = selectedAction	== FileOperation::None;

	QModelIndex	oldIndex = index(oldIsNone ? 0 : _opToIndex[_selected]),
				newIndex = index(newIsNone ? 0 : _opToIndex[selectedAction]);

	_selected = selectedAction;

	if(!oldIsNone)	emit dataChanged(oldIndex, oldIndex);
	if(!newIsNone)	emit dataChanged(newIndex, newIndex);

	emit selectedActionChanged(_selected);
}

void ActionButtons::refresh()
{
	beginResetModel();

	std::vector<DataRow> savedata = _data;

	loadButtonData(_data);

	for(int i=0 ; i < savedata.size() ; i++)
		_data[i].enabled = savedata[i].enabled;

	endResetModel();
}

std::set<ResourceButtons::ButtonType> ActionButtons::resourceButtonsForButton(FileOperation button)
{
	if(button == None) return {};
	return _data[_opToIndex[button]].resourceButtons;
}

void ActionButtons::selectButtonUp()
{
	int idx = _selected == None ? 1 : _opToIndex[_selected];

	for(int move = 1; move < _data.size(); move++)
	{
		size_t idxUp = ((_data.size() + idx) - move) % _data.size();

		if(_data[idxUp].enabled)
		{
			setSelectedAction(_data[idxUp].operation);
			return;
		}
	}
}

void ActionButtons::selectButtonDown()
{
	int idx = _selected == None ? _data.size() - 1 : _opToIndex[_selected];

	for(int move = 1; move < _data.size(); move++)
	{
		size_t idxDown = (idx + move) % _data.size();

		if(_data[idxDown].enabled)
		{
			setSelectedAction(_data[idxDown].operation);
			return;
		}
	}
}
