#include "availablefields.h"

#include <boost/foreach.hpp>
#include <boost/bind.hpp>
#include "qutils.h"

using namespace std;

AvailableFields::AvailableFields(QObject *parent)
	: QAbstractListModel(parent)
{
	_shouldFilter = false;

	_nominalTextIcon = QIcon(":/icons/variable-nominal-text.svg");
	_nominalIcon = QIcon(":/icons/variable-nominal.svg");
	_ordinalIcon = QIcon(":/icons/variable-ordinal.svg");
	_scaleIcon = QIcon(":/icons/variable-scale.svg");
}

void AvailableFields::setDataSet(DataSet *dataSet)
{
	_dataSet = dataSet;

	updateAvailableFields();
}

void AvailableFields::filter(std::vector<string> show)
{
	_filter = show;
	_shouldFilter = true;

	updateAvailableFields();
}

void AvailableFields::provideFor(OptionVariables *option)
{
	_provideFor.push_back(option);
	option->changed.connect(boost::bind(&AvailableFields::updateAvailableFields, this));
}

QStringList AvailableFields::getFields(QModelIndexList indices)
{
	QStringList fields;

	BOOST_FOREACH(QModelIndex &index, indices)
		fields.append(_availableFields.at(index.row()));

	return fields;
}

void AvailableFields::updateAvailableFields()
{
	beginResetModel();

	QStringList availableFields;

	BOOST_FOREACH(Column &column, _dataSet->columns())
	{
		string n = column.name();
		if (( ! _shouldFilter) || std::find(_filter.begin(), _filter.end(), n) != _filter.end())
		{
			QString name = QString::fromUtf8(n.c_str(), n.length());
			availableFields.append(name);
		}
	}

	BOOST_FOREACH(OptionVariables *option, _provideFor)
	{
		BOOST_FOREACH(string assigned, option->variables())
		{
			QString value = QString::fromUtf8(assigned.c_str(), assigned.length());
			availableFields.removeOne(value);
		}
	}

	_availableFields = availableFields;

	endResetModel();
}


QVariant AvailableFields::data(const QModelIndex &index, int role) const
{
	int row = index.row();

	if (role == Qt::DisplayRole)
	{
		return QVariant(_availableFields.at(row));
	}
	else if (role == Qt::DecorationRole)
	{
		string variable = fq(_availableFields.at(row));
		Column &column = _dataSet->columns().get(variable);

		switch (column.columnType())
		{
		case Column::ColumnTypeNominalText:
			return QVariant(_nominalTextIcon);
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

QStringList AvailableFields::available()
{
	return _availableFields;
}


int AvailableFields::rowCount(const QModelIndex &) const
{
	return _availableFields.length();
}
