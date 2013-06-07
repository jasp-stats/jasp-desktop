#include "availablefields.h"

#include "boost/foreach.hpp"
#include "boost/bind.hpp"

AvailableFields::AvailableFields(QObject *parent = 0)
	: QStringListModel(parent)
{
}

void AvailableFields::setDataSet(DataSet *dataSet)
{
	_dataSet = dataSet;

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
	{
		fields.append(this->stringList().at(index.row()));
	}

	return fields;
}

void AvailableFields::updateAvailableFields()
{
	QStringList availableFields;

	BOOST_FOREACH(Column &column, _dataSet->columns())
	{
		QString name = QString::fromUtf8(column.name().c_str(), column.name().length());
		availableFields.append(name);
	}

	BOOST_FOREACH(OptionFields *option, _provideFor)
	{
		BOOST_FOREACH(string assigned, option->value())
		{
			QString value = QString::fromUtf8(assigned.c_str(), assigned.length());
			availableFields.removeOne(value);
		}
	}

	setStringList(availableFields);
}
