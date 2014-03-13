#include "availablefields.h"

#include "boost/foreach.hpp"
#include "boost/bind.hpp"

using namespace std;

AvailableFields::AvailableFields(QObject *parent)
	: QAbstractListModel(parent)
{
	_shouldFilter = false;

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

void AvailableFields::provideFor(OptionFields *option)
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

	BOOST_FOREACH(OptionFields *option, _provideFor)
	{
		BOOST_FOREACH(string assigned, option->value())
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
		QString variable = _availableFields.at(row);
		QByteArray utf8 = variable.toUtf8();
		string n(utf8.constData(), utf8.length());
		Column *column = _dataSet->columns().get(n);

		switch (column->columnType())
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

QStringList AvailableFields::available()
{
	return _availableFields;
}


int AvailableFields::rowCount(const QModelIndex &) const
{
	return _availableFields.length();
}
