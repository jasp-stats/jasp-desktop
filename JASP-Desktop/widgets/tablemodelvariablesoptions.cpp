#include "tablemodelvariablesoptions.h"

#include <boost/foreach.hpp>
#include "options/optionfield.h"
#include "options/optionlist.h"

using namespace std;

TableModelVariablesOptions::TableModelVariablesOptions(QObject *parent) :
	QAbstractTableModel(parent)
{
	_boundTo = NULL;
}

int TableModelVariablesOptions::rowCount(const QModelIndex &) const
{
	if (_boundTo == NULL)
		return 0;

	return _boundTo->size();
}

int TableModelVariablesOptions::columnCount(const QModelIndex &parent) const
{
	if (_boundTo == NULL)
		return 0;

	return _boundTo->rowTemplate()->size();
}

QVariant TableModelVariablesOptions::data(const QModelIndex &index, int role) const
{
	if (_boundTo == NULL)
		return QVariant();

	if (index.column() == 0)
	{
		if (role == Qt::DisplayRole)
		{
			string value = _boundTo->at(index.row())->variable();
			return QString::fromUtf8(value.c_str(), value.length());
		}
		else
		{
			return QVariant();
		}
	}
	else
	{
		if (role != Qt::DisplayRole && role != Qt::EditRole)
			return QVariant();

		Option* option = _boundTo->at(index.row())->get(index.column());
		OptionList *list = dynamic_cast<OptionList *>(option);
		if (list == NULL)
			return QVariant("WTF");

		string selected = list->value();
		QString qsSelected = QString::fromUtf8(selected.c_str(), selected.length());

		if (role == Qt::DisplayRole)
			return qsSelected;

		vector<string> items = list->options();
		QStringList qsItems;

		for (int i = 0; i < items.size(); i++)
		{
			string item = items.at(i);
			QString qs = QString::fromUtf8(item.c_str(), item.size());
			qsItems.append(qs);
		}

		QVariant qvSelected = QVariant(qsSelected);
		QVariant qvItems = QVariant(qsItems);

		QList<QVariant> qvValue;
		qvValue.append(qvSelected);
		qvValue.append(qvItems);

		return qvValue;
	}

}

bool TableModelVariablesOptions::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (_boundTo == NULL)
		return false;

	Options *options = _boundTo->at(index.row());
	Option *option = options->get(index.column());
	OptionList *list = dynamic_cast<OptionList *>(option);

	QString qsValue = value.toString();
	QByteArray bytes = qsValue.toUtf8();

	list->setValue(string(bytes.constData(), bytes.length()));

	emit dataChanged(index, index);

	return true;
}

Qt::ItemFlags TableModelVariablesOptions::flags(const QModelIndex &index) const
{
	if (_boundTo == NULL)
		return 0;

	if (index.column() == 0)
		return Qt::ItemIsEnabled;

	return Qt::ItemIsEnabled | Qt::ItemIsEditable;
}

QVariant TableModelVariablesOptions::headerData(int section, Qt::Orientation orientation, int role) const
{
	if (_boundTo == NULL)
		return QVariant();

	if (role == Qt::DisplayRole && orientation == Qt::Horizontal)
	{
		string name;
		Option *option;
		_boundTo->rowTemplate()->get(section, name, option);
		QString qsName = QString::fromUtf8(name.c_str(), name.length());
		return qsName;
	}

	return QVariant();
}

vector<string> TableModelVariablesOptions::getVariableNames(const QList<ColumnInfo> &variables)
{
	vector<string> values;

	foreach (const ColumnInfo &info, variables)
	{
		QByteArray bytes = info.first.toUtf8();
		string name(bytes.constData(), bytes.length());


		values.push_back(name);
	}

	return values;
}

void TableModelVariablesOptions::setVariables(const QList<ColumnInfo> &variables)
{
	if (_boundTo == NULL)
		return;

	beginResetModel();

	vector<string> variableNames = getVariableNames(variables);

	int i = 0;
	while (i < _boundTo->size())
	{
		string existingName = _boundTo->at(i)->variable();

		vector<string>::iterator itr = find(variableNames.begin(), variableNames.end(), existingName);

		if (itr == variableNames.end())
			_boundTo->removeAt(i);
		else
			i++;
	}

	for (int i = 0; i < variableNames.size(); i++)
	{
		string variableName = variableNames[i];

		if (_boundTo->contains(variableName))
		{
			if (_boundTo->at(i)->variable() == variableName)
				continue;

			OptionsRow *row = _boundTo->remove(variableName);
			_boundTo->insertAt(row, i);
		}
		else
		{
			_boundTo->insertAt(variableName, i);
		}
	}

	endResetModel();
}

void TableModelVariablesOptions::bindTo(Option *option)
{
	beginResetModel();
	_boundTo = dynamic_cast<OptionsTable *>(option);
	endResetModel();
}
