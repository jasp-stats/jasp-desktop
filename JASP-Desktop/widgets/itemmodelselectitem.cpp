#include "itemmodelselectitem.h"

#include "utils.h"

using namespace std;

ItemModelSelectItem::ItemModelSelectItem()
{
	_boundTo = NULL;
	_selectedIndex = -1;
}

void ItemModelSelectItem::bindTo(Option *option)
{
	beginResetModel();

	_boundTo = dynamic_cast<OptionList *>(option);
	_selectedIndex = -1;

	if (_boundTo == NULL)
		return;

	vector<string> options = _boundTo->options();

	for (int i = 0; i < options.size(); i++)
	{
		if (options.at(i) == _boundTo->value())
		{
			_selectedIndex = i;
			break;
		}
	}

	endResetModel();
}

QVariant ItemModelSelectItem::data(const QModelIndex &index, int role) const
{
	if (_boundTo != NULL && role == Qt::CheckStateRole)
	{
		return index.row() == _selectedIndex;
	}
	else
	{
		return QStandardItemModel::data(index, role);
	}
}

bool ItemModelSelectItem::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (_boundTo != NULL && index.isValid() && role == Qt::CheckStateRole && value.canConvert(QVariant::Bool))
	{
		bool checked = value.toBool();

		if (checked)
		{
			setSelected(index.row());
		}
		else
		{
			if (_selectedIndex != -1)
			{
				int oldSelectedIndex = _selectedIndex;
				_selectedIndex = -1;
				_boundTo->setValue("");
				emit dataChanged(createIndex(oldSelectedIndex, 0), createIndex(oldSelectedIndex, 0), QVector<int>(1, Qt::CheckStateRole));
			}
		}

		return true;
	}
	else
	{
		return QStandardItemModel::setData(index, value, role);
	}
}

Qt::ItemFlags ItemModelSelectItem::flags(const QModelIndex &index) const
{
	if ( ! index.isValid())
		return Qt::ItemIsEnabled;

	return Qt::ItemIsUserCheckable | Qt::ItemIsEnabled | Qt::ItemIsSelectable;
}

int ItemModelSelectItem::selectedIndex()
{
	return _selectedIndex;
}

void ItemModelSelectItem::setSelected(int index)
{
	int oldSelectedIndex = _selectedIndex;
	_selectedIndex = index;
	string value = _boundTo->options().at(_selectedIndex);
	_boundTo->setValue(value);

	emit dataChanged(createIndex(oldSelectedIndex, 0), createIndex(oldSelectedIndex, 0), QVector<int>(1, Qt::CheckStateRole));
	emit dataChanged(createIndex(_selectedIndex,   0), createIndex(_selectedIndex,   0), QVector<int>(1, Qt::CheckStateRole));
}

