
#include "tablemodelanovadesign.h"

#include <QSize>
#include <QDebug>
#include <QIcon>
#include <QBrush>

#include "qutils.h"
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

	return 2;
}

QVariant TableModelAnovaDesign::data(const QModelIndex &index, int role) const
{
	if (_boundTo == NULL)
		return QVariant();

	Row row = _rows.at(index.row());

	if (index.column() == 0)
	{
		if (role == Qt::DisplayRole)
		{
			return row.text();
		}
		else if (role == Qt::EditRole)
		{
			if (row.isHypothetical())
				return "";
			else
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
	}
	else
	{
		if (role == Qt::DecorationRole)
		{
			if (index.row() < 3 || row.isHypothetical() || row.subIndex() == 0 || row.subIndex() == 1)
			{
				return QVariant();
			}
			else
			{
				static QIcon icon(":/icons/dialog-close.png");
				return icon;
			}
		}
	}

	return QVariant();
}

QVariant TableModelAnovaDesign::headerData(int section, Qt::Orientation orientation, int role) const
{
	if (orientation == Qt::Horizontal)
	{
		if (role == Qt::SizeHintRole)
		{
			if (section == 0)
				return QSize(-1, 24);
			else
				return QSize(24, 24);
		}
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

		OptionVariables *variablesOption = static_cast<OptionVariables *>(group->get("levels"));

		_rows.append(Row(tq(oldName), false, i));

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

Qt::ItemFlags TableModelAnovaDesign::flags(const QModelIndex &index) const
{
	if (index.isValid() == false)
	{
		return Qt::ItemIsEnabled;
	}
	else if (index.column() == 0)
	{
		return Qt::ItemIsEnabled | Qt::ItemIsSelectable | Qt::ItemIsEditable;
	}
	else
	{
		Row row = _rows.at(index.row());

		if (index.row() < 3 || row.isHypothetical() || row.subIndex() == 0 || row.subIndex() == 1)
		{
			return Qt::ItemIsEnabled;
		}
		else
		{
			return Qt::ItemIsEnabled | Qt::ItemIsSelectable;
		}
	}
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

bool TableModelAnovaDesign::removeRows(int row, int, const QModelIndex &)
{
	// count is ignored, because it will never be more than 1

	if (row >= 3 && _rows.at(row).isHypothetical() == false)
		deleteRow(row);

	return false; // return false, because we handle the refresh ourselves
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

	if (row.isHypothetical() == false && row.text() == tq(value))
		return;

	if (row.isHeading())
	{
		QString originalName = tq(value).trimmed();
		QString name = originalName;
		int n = 2;

		while (true)
		{
			bool unique = true;

			for (int i = 0; i < _rows.length(); i++)
			{
				if (i == rowNo)
					continue;

				Row &existing = _rows[i];

				if (existing.isHeading() == false || existing.isHypothetical())
					continue;

				if (existing.text() == name)
				{
					unique = false;
					break;
				}
			}

			if (unique)
				break;
			else
				name = QString("%1 (%2)").arg(originalName).arg(n++);
		}

		if (row.isHypothetical())
		{
			Options *newRow = static_cast<Options *>(_boundTo->rowTemplate()->clone());
			OptionString *factorName = static_cast<OptionString *>(newRow->get("name"));
			factorName->setValue(fq(name));

			_groups.push_back(newRow);

			Terms terms;
			terms.add(Term(name));

			emit factorAdded(terms);
		}
		else
		{
			OptionString *option = static_cast<OptionString *>(_groups.at(row.index())->get("name"));

			Terms old;
			old.add(Term(option->value()));

			Terms n3w;
			n3w.add(Term(name));

			option->setValue(fq(name));

			emit factorRemoved(old);
			emit factorAdded(n3w);
		}
	}
	else
	{
		OptionVariables *option = static_cast<OptionVariables *>(_groups.at(row.index())->get("levels"));
		vector<string> levels = option->variables();

		string originalName = fq(tq(value).trimmed());
		string name = originalName;
		int n = 2;

		while (true)
		{
			bool unique = true;

			for (int i = 0; i < levels.size(); i++)
			{
				if (i == row.subIndex())
					continue;

				string &level = levels[i];

				if (level == name)
				{
					unique = false;
					break;
				}
			}

			if (unique)
				break;
			else
				name = fq(QString("%1 (%2)").arg(tq(originalName)).arg(n++));
		}

		if (row.isHypothetical())
			levels.push_back(name);
		else
			levels[row.subIndex()] = name;

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

			Terms removed;
			removed.add(Term(row.text()));

			emit factorRemoved(removed);

			_groups.erase(itr);
		}
		else
		{
			OptionString *factorNameTemplate = static_cast<OptionString *>(_boundTo->rowTemplate()->get("name"));
			string defaultName = fq(tq(factorNameTemplate->value()).arg(rowNo + 1));
			OptionString *factorNameOption = static_cast<OptionString *>(_groups.at(0)->get("name"));

			Terms old;
			old.add(factorNameOption->value());

			Terms n3w;
			n3w.add(defaultName);

			emit factorRemoved(old);
			emit factorAdded(n3w);

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

