
#include "tablemodelanovadesign.h"

#include <QSize>
#include <QDebug>
#include <QIcon>
#include <QBrush>

#include "utils.h"
#include "variableinfo.h"
#include "column.h"

using namespace std;

TableModelAnovaDesign::TableModelAnovaDesign(QObject *parent)
	: TableModel(parent)
{
	_boundTo = NULL;
}

void TableModelAnovaDesign::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable *>(option);

	if (_boundTo != NULL)
	{
		_groups = _boundTo->value();
		refresh();
		emit designChanged();
	}
}

int TableModelAnovaDesign::rowCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);

	if (_boundTo == NULL)
		return 0;

	return _rows.length();
}

int TableModelAnovaDesign::columnCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);

	if (_boundTo == NULL)
		return 0;

	return 1;
}

QVariant TableModelAnovaDesign::data(const QModelIndex &index, int role) const
{
	if (_boundTo == NULL)
		return QVariant();

	Row row = _rows.at(index.row());

	if (role == Qt::DisplayRole)
	{
		return row.text();
	}
	else if (role == Qt::EditRole)
	{
		return row.text();
	}
	else if (role == Qt::ForegroundRole)
	{
		if (row.isHypothetical())
			return QBrush(QColor(0xCC, 0xCC, 0xCC));
		else
			return QVariant();
	}
	else if (role == Qt::TextAlignmentRole)
	{
		if (row.isHeading())
			return Qt::AlignCenter;
		else
			return QVariant();
	}
	else if (role == Qt::SizeHintRole)
	{
		if (row.isHeading())
			return QSize(-1, 24);
	}

	return QVariant();
}

void TableModelAnovaDesign::refresh()
{
	_rows.clear();

	if (_boundTo == NULL)
		return;

	beginResetModel();

	int i;

	OptionString *nameTemplateOption = static_cast<OptionString *>(_boundTo->rowTemplate()->get("name"));
	QString nameTemplate = tq(nameTemplateOption->value());

	for (i = 0; i < _groups.size(); i++)
	{
		Options *group = _groups.at(i);
		OptionString *nameOption = static_cast<OptionString *>(group->get("name"));
		string oldName = nameOption->value();
		string newName = fq(nameTemplate.arg(i + 1));

		if (oldName != newName)
			nameOption->setValue(newName);

		OptionVariables *variablesOption = static_cast<OptionVariables *>(group->get("levels"));

		_rows.append(Row(tq(newName), false, i));

		vector<string> variables = variablesOption->variables();

		int j;
		for (j = 0; j < variables.size(); j++)
			_rows.append(Row(tq(variables.at(j)), false, i, j));

		_rows.append(Row(QString("Level %1").arg(j + 1), true, i, j));
	}

	QString name = nameTemplate.arg(i + 1);
	_rows.append(Row(name, true, i));

	endResetModel();
}

Qt::ItemFlags TableModelAnovaDesign::flags(const QModelIndex &) const
{
	return Qt::ItemIsEnabled | Qt::ItemIsSelectable | Qt::ItemIsEditable;
}

bool TableModelAnovaDesign::setData(const QModelIndex &index, const QVariant &value, int)
{
	string v = fq(value.toString());

	if (v == "")
		deleteRow(index.row());
	else
		changeRow(index.row(), v);

	return false;
}

QList<Factor> TableModelAnovaDesign::design()
{
	QList<Factor> factors;

	for (uint i = 0; i < _groups.size(); i++)
	{
		Options *factorOptions = _groups.at(i);
		OptionString *factorNameOption = static_cast<OptionString *>(factorOptions->get("name"));
		OptionVariables *factorLevelsOption = static_cast<OptionVariables *>(factorOptions->get("levels"));

		Factor factor;

		factor.first = tq(factorNameOption->value());
		factor.second = tql(factorLevelsOption->variables());

		factors.append(factor);
	}

	return factors;
}

void TableModelAnovaDesign::changeRow(int rowNo, string value)
{
	const Row &row = _rows.at(rowNo);

	if (row.isHeading())
	{
		if (row.isHypothetical())
		{
			Options *newRow = static_cast<Options *>(_boundTo->rowTemplate()->clone());
			OptionString *factorName = static_cast<OptionString *>(newRow->get("name"));
			factorName->setValue(value);
			_groups.push_back(newRow);
		}
		else
		{
			OptionString *option = static_cast<OptionString *>(_groups.at(row.index())->get("name"));
			option->setValue(value);
		}
	}
	else
	{
		OptionVariables *option = static_cast<OptionVariables *>(_groups.at(row.index())->get("levels"));
		vector<string> levels = option->variables();

		if (row.isHypothetical())
			levels.push_back(value);
		else
			levels[row.subIndex()] = value;

		option->setValue(levels);
	}

	refresh();
	_boundTo->setValue(_groups);
	emit designChanged();
}

void TableModelAnovaDesign::deleteRow(int rowNo)
{
	const Row &row = _rows.at(rowNo);

	if (row.isHypothetical())
	{
		return;
	}
	else if (row.isHeading())
	{
		if (_groups.size() > 1)
		{
			std::vector<Options *>::iterator itr = _groups.begin();

			for (int i = 0; i < row.index(); i++)
				itr++;

			_groups.erase(itr);
		}
		else
		{
			OptionString *factorNameTemplate = static_cast<OptionString *>(_boundTo->rowTemplate()->get("name"));
			string defaultName = fq(tq(factorNameTemplate->value()).arg(rowNo + 1));
			OptionString *factorNameOption = static_cast<OptionString *>(_groups.at(0)->get("name"));
			factorNameOption->setValue(defaultName);
		}
	}
	else
	{
		OptionVariables *option = static_cast<OptionVariables *>(_groups.at(row.index())->get("levels"));
		vector<string> levels = option->variables();

		if (levels.size() > 2)
		{
			vector<string>::iterator itr = levels.begin();

			for (int i = 0; i < row.subIndex(); i++)
				itr++;

			levels.erase(itr);
		}
		else
		{
			string defaultName = fq(QString("Level %1").arg(row.subIndex() + 1));
			levels[row.subIndex()] = defaultName;
		}

		option->setValue(levels);
	}

	refresh();
	_boundTo->setValue(_groups);
	emit designChanged();
}

