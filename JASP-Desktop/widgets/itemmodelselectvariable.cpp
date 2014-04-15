#include "itemmodelselectvariable.h"

#include "utils.h"
#include "column.h"

using namespace std;

ItemModelSelectVariable::ItemModelSelectVariable(QObject *parent) :
	QAbstractListModel(parent)
{
	_source = NULL;
	_boundTo = NULL;
	_selectedIndex = 0;

	_nominalIcon = QIcon(":/icons/variable-nominal.svg");
	_ordinalIcon = QIcon(":/icons/variable-ordinal.svg");
	_scaleIcon = QIcon(":/icons/variable-scale.svg");
}

int ItemModelSelectVariable::rowCount(const QModelIndex &parent) const
{
	if (_source == NULL)
		return 1;

	return _source->allVariables().length() + 1;
}

QVariant ItemModelSelectVariable::data(const QModelIndex &index, int role) const
{
	if (_source == NULL)
		return QVariant();

	if (role == Qt::DisplayRole)
	{
		if (index.row() == 0)
			return "[ None ]";
		else
			return _source->allVariables().at(index.row() - 1).first;
	}
	else if (role == Qt::CheckStateRole)
	{
		return index.row() == _selectedIndex;
	}
	else if (role == Qt::DecorationRole)
	{
		if (index.row() == 0)
			return QVariant();

		switch (_source->allVariables().at(index.row() - 1).second)
		{
		case Column::ColumnTypeNominal:
			return QVariant(_nominalIcon);
		case Column::ColumnTypeOrdinal:
			return QVariant(_ordinalIcon);
		case Column::ColumnTypeScale:
			return QVariant(_scaleIcon);
		default:
			return QVariant();
		}
	}
	else
	{
		return QVariant();
	}
}

Qt::ItemFlags ItemModelSelectVariable::flags(const QModelIndex &index) const
{
	if ( ! index.isValid())
		return Qt::ItemIsEnabled;

	return Qt::ItemIsUserCheckable | Qt::ItemIsEnabled | Qt::ItemIsSelectable;
}

bool ItemModelSelectVariable::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (_boundTo != NULL && index.isValid() && role == Qt::CheckStateRole && value.canConvert(QVariant::Bool))
	{
		bool checked = value.toBool();

		if (checked)
		{
			_selectedIndex = index.row();
			if (_selectedIndex == 0)
				_boundTo->setValue("");
			else
				_boundTo->setValue(fq(_source->allVariables().at(_selectedIndex - 1).first));
		}

		return true;
	}
	else
	{
		return false;
	}
}

void ItemModelSelectVariable::bindTo(Option *option)
{
	beginResetModel();

	_boundTo = dynamic_cast<OptionField *>(option);
	updateSelected();

	endResetModel();
}

void ItemModelSelectVariable::setSource(ListModelVariablesAvailable *source)
{
	beginResetModel();
	_source = source;
	updateSelected();
	connect(_source, SIGNAL(variablesChanged()), this, SLOT(variablesChangedHandler()));
	endResetModel();
}

void ItemModelSelectVariable::variablesChangedHandler()
{
	beginResetModel();
	updateSelected();
	endResetModel();
}

void ItemModelSelectVariable::updateSelected()
{
	/*if (_source == NULL || _boundTo == NULL)
		return;

	_selectedIndex = 0;

	for (int i = 0; i < _source->allVariables().size(); i++)
	{
		if (fq(_source->allVariables().at(i).first) == _boundTo->value()[0])
		{
			_selectedIndex = i + 1;
			break;
		}
	}*/

}


