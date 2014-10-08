#include "tablemodelvariableslevels.h"

#include <QMimeData>

#include "options/optionstring.h"
#include "options/optionlist.h"
#include "options/optionvariables.h"
#include "column.h"

using namespace std;

TableModelVariablesLevels::TableModelVariablesLevels(QWidget *parent) :
	TableModel(parent)
{
	_boundTo = NULL;
	_source = NULL;

	_variableTypesSuggested = 0;
	_variableTypesAllowed = 0xff;

	_nominalTextIcon = QIcon(":/icons/variable-nominal-text.svg");
	_nominalIcon = QIcon(":/icons/variable-nominal.svg");
	_ordinalIcon = QIcon(":/icons/variable-ordinal.svg");
	_scaleIcon = QIcon(":/icons/variable-scale.svg");
}

void TableModelVariablesLevels::bindTo(Option *option)
{
	if (_source == NULL)
	{
		qDebug() << "source not set!";
		return;
	}

	_boundTo = dynamic_cast<OptionsTable *>(option);

	refresh();
}

void TableModelVariablesLevels::mimeDataMoved(const QModelIndexList &indexes)
{
	bool levelRemoved = false;

	Terms toRemove;

	foreach (const QModelIndex &index, indexes)
		toRemove.add(_rows.at(index.row()).title());

	for (int i = _levels.size() - 1; i >= 0; i--)
	{
		Options *levelOptions = _levels.at(i);
		OptionVariables *variablesOption = dynamic_cast<OptionVariables *>(levelOptions->get("variables"));
		Terms variables = variablesOption->variables();

		variables.remove(toRemove);

		if (variables.size() == 0)
		{
			Options *options = _levels.at(i);
			_levels.erase(find(_levels.begin(), _levels.end(), options));
			delete options;
			levelRemoved = true;
		}
		else
		{
			variablesOption->setValue(variables.asVector());
		}
	}

	if (levelRemoved)
	{
		OptionString *nameOption = static_cast<OptionString *>(_boundTo->rowTemplate()->get("name"));
		QString nameTemplate = tq(nameOption->value());

		for (uint i = 0; i < _levels.size(); i++)
		{
			nameOption = static_cast<OptionString *>(_levels.at(i)->get("name"));
			nameOption->setValue(fq(nameTemplate.arg(i + 1)));
		}
	}

	_boundTo->setValue(_levels);

	refresh();

}

void TableModelVariablesLevels::setVariableTypesSuggested(int variableTypesSuggested)
{
	_variableTypesSuggested = variableTypesSuggested;
}

int TableModelVariablesLevels::variableTypesSuggested() const
{
	return _variableTypesSuggested;
}

void TableModelVariablesLevels::setVariableTypesAllowed(int variableTypesAllowed)
{
	_variableTypesAllowed = variableTypesAllowed;
}

int TableModelVariablesLevels::variableTypesAllowed() const
{
	return _variableTypesAllowed;
}

bool TableModelVariablesLevels::isSuggested(const Term &term) const
{
	QVariant v = requestInfo(term, VariableInfo::VariableType);
	int variableType = v.toInt();

	return variableType & _variableTypesSuggested;
}

bool TableModelVariablesLevels::isAllowed(const Term &term) const
{
	QVariant v = requestInfo(term, VariableInfo::VariableType);
	int variableType = v.toInt();

	return variableType == 0 || variableType & _variableTypesAllowed;
}

int TableModelVariablesLevels::rowCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);

	if (_boundTo == NULL)
		return 0;

	return _rows.length();
}

int TableModelVariablesLevels::columnCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);

	if (_boundTo == NULL)
		return 0;

	return 1;
}

QVariant TableModelVariablesLevels::data(const QModelIndex &index, int role) const
{
	if (_boundTo == NULL)
		return QVariant();

	Row row = _rows.at(index.row());

	if (role == Qt::DisplayRole || role == Qt::EditRole)
	{
		if ( ! row.isOption())
		{
			return row.title();
		}
		else
		{
			OptionList *list = row.option();
			QString selected = tq(list->value());

			if (role == Qt::DisplayRole)
				return selected;

			QStringList items = tql(list->options());

			QList<QVariant> value;
			value.append(selected);
			value.append(items);

			return value;
		}
	}
	else if (role == Qt::DecorationRole)
	{
		if (row.isHeading() == false && row.isOption() == false)
		{
			int variableType = requestInfo(row.title(), VariableInfo::VariableType).toInt();

			switch (variableType)
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
	}
	else if (role == Qt::ForegroundRole)
	{
		if (index.row() == _rows.length() - 1)
			return QBrush(QColor(0xAA, 0xAA, 0xAA));
	}
	else if (role == Qt::TextAlignmentRole)
	{
		if (row.isHeading())
			return Qt::AlignCenter;
		else if (row.isOption())
			return Qt::AlignTop;
	}
	else if (role == Qt::SizeHintRole)
	{
		if (row.isHeading())
			return QSize(-1, 24);
	}

	return QVariant();
}

Qt::ItemFlags TableModelVariablesLevels::flags(const QModelIndex &index) const
{
	Qt::ItemFlags flags = Qt::ItemIsEnabled;

	if (index.isValid() == false)
	{
		flags |= Qt::ItemIsDropEnabled;
	}
	else
	{
		Row row = _rows.at(index.row());
		if (row.isOption())
			flags |= Qt::ItemIsEditable;
		else if (row.isHeading() == false)
			flags |= Qt::ItemIsSelectable | Qt::ItemIsDragEnabled;
	}

	return flags;
}

bool TableModelVariablesLevels::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (role != Qt::DisplayRole && role != Qt::EditRole)
		return false;

	if (_boundTo == NULL)
		return false;

	OptionList *list = _rows.at(index.row()).option();

	QString qsValue = value.toString();
	QByteArray bytes = qsValue.toUtf8();

	list->setValue(string(bytes.constData(), bytes.length()));

	_boundTo->setValue(_levels);

	emit dataChanged(index, index);

	return true;
}

Qt::DropActions TableModelVariablesLevels::supportedDropActions() const
{
	return Qt::MoveAction;
}

Qt::DropActions TableModelVariablesLevels::supportedDragActions() const
{
	return Qt::MoveAction;
}

QStringList TableModelVariablesLevels::mimeTypes() const
{
	QStringList m;
	m << "application/vnd.list.variable";
	return m;
}

QMimeData *TableModelVariablesLevels::mimeData(const QModelIndexList &indexes) const
{
	QMimeData *mimeData = new QMimeData();
	QByteArray encodedData;

	QDataStream dataStream(&encodedData, QIODevice::WriteOnly);

	dataStream << indexes.length();

	for (int i = 0; i < indexes.length(); i++)
	{
		Row row = _rows.at(indexes.at(i).row());
		dataStream << row.title();
	}

	mimeData->setData("application/vnd.list.variable", encodedData);

	return mimeData;
}

bool TableModelVariablesLevels::dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent)
{
	if (action == Qt::IgnoreAction)
		return true;

	if ( ! canDropMimeData(data, action, row, column, parent))
		return false;

	if (mimeTypes().contains("application/vnd.list.variable"))
	{
		QByteArray encodedData = data->data("application/vnd.list.variable");

		Terms variables;
		variables.set(encodedData);

		if (variables.size() == 0)
			return false;

		if (parent.isValid() == false)
		{
			if (row == -1)  // dropped at end
				row = _rows.length();
			else if (row == 0)  // dropped at beginning
				row = 1;

			int level = 0;
			int positionInLevel = 0;
			for (int i = 1; i < row; i++)
			{
				if (_rows[i].isHeading())
				{
					level++;
					positionInLevel = 0;
				}
				else if (_rows[i].isOption() == false)
				{
					positionInLevel++;
				}
			}

			if (level == _levels.size())
			{
				Options *options = dynamic_cast<Options*>(_boundTo->rowTemplate()->clone());
				OptionString *nameOption = dynamic_cast<OptionString *>(options->get("name"));
				string levelName = fq(tq(nameOption->value()).arg(level + 1));
				nameOption->setValue(levelName);
				_levels.push_back(options);
			}

			Options *levelOption = _levels.at(level);
			OptionVariables *variablesOption = dynamic_cast<OptionVariables*>(levelOption->get("variables"));
			vector<string> currentVariables = variablesOption->variables();

			vector<string>::iterator insertionPoint = currentVariables.begin();
			for (int i = 0; i < positionInLevel; i++)
				insertionPoint++;

			foreach (const Term &variable, variables)
			{
				currentVariables.insert(insertionPoint, variable.asString());
				insertionPoint++;
			}

			variablesOption->setValue(currentVariables);

			_boundTo->setValue(_levels);

			refresh();

			return true;
		}


	}

	return false;
}

bool TableModelVariablesLevels::canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const
{
	Q_UNUSED(action);
	Q_UNUSED(row);
	Q_UNUSED(column);
	Q_UNUSED(parent);

	if (data->hasFormat("application/vnd.list.variable"))
	{
		QByteArray encodedData = data->data("application/vnd.list.variable");

		Terms variables;
		variables.set(encodedData);

		foreach (const Term &variable, variables)
		{
			if ( ! isAllowed(variable))
				return false;
		}

		return true;
	}
	else
	{
		return false;
	}

	return false;
}

void TableModelVariablesLevels::setSource(TableModelVariablesAvailable *source)
{
	_source = source;
	setInfoProvider(source);
}

void TableModelVariablesLevels::refresh()
{
	_rows.clear();

	if (_boundTo == NULL)
		return;

	beginResetModel();

	uint i;

	for (i = 0; i < _levels.size(); i++)
	{
		Options *level = _levels.at(i);
		OptionVariables *variablesOption = dynamic_cast<OptionVariables *>(level->get("variables"));
		OptionString *nameOption = dynamic_cast<OptionString *>(level->get("name"));
		vector<string> variables = variablesOption->variables();

		_rows.append(Row(tq(nameOption->value()), true));

		for (uint j = 2; j < level->size(); j++)
		{
			OptionList *list = dynamic_cast<OptionList *>(level->get(j));
			_rows.append(Row(list));
		}

		foreach (const string &variable, variables)
			_rows.append(Row(tq(variable)));
	}

	OptionString *nameTemplate = static_cast<OptionString *>(_boundTo->rowTemplate()->get("name"));
	QString name = tq(nameTemplate->value()).arg(i + 1);
	_rows.append(Row(name, true));

	endResetModel();
}

