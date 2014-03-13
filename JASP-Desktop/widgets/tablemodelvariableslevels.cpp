#include "tablemodelvariableslevels.h"

#include <QMimeData>

#include "options/optionstring.h"
#include "options/optionlist.h"
#include "options/optionfields.h"
#include "column.h"


typedef QPair<QString, int> ColumnInfo;

using namespace std;

TableModelVariablesLevels::TableModelVariablesLevels(QWidget *parent) :
	TableModel(parent)
{
	_boundTo = NULL;
	_source = NULL;

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

	readFromOption();
}

void TableModelVariablesLevels::mimeDataMoved(const QModelIndexList &indexes)
{
	bool levelRemoved = false;

	for (int i = _boundTo->size() - 1; i >= 0; i--)
	{
		Options *levelOptions = _boundTo->at(i);
		OptionFields *variablesOption = dynamic_cast<OptionFields *>(levelOptions->get("variables"));
		vector<string> variables = variablesOption->value();
		bool changed = false;

		foreach (const QModelIndex &index, indexes)
		{
			string value = fq(_rows.at(index.row()).name());
			if (remove(variables, value))
				changed = true;
		}

		if (changed)
		{
			if (variables.size() == 0)
			{
				Options *options = _boundTo->remove(i);
				delete options;
				levelRemoved = true;
			}
			else
			{
				variablesOption->setValue(variables);
			}
		}
	}

	if (levelRemoved)
	{
		OptionString *nameOption = static_cast<OptionString *>(_boundTo->rowTemplate()->get("name"));
		QString nameTemplate = tq(nameOption->value());

		for (int i = 0; i < _boundTo->size(); i++)
		{
			nameOption = static_cast<OptionString *>(_boundTo->at(i)->get("name"));
			nameOption->setValue(fq(nameTemplate.arg(i + 1)));
		}
	}

	readFromOption();

}

int TableModelVariablesLevels::rowCount(const QModelIndex &parent) const
{
	if (_boundTo == NULL)
		return 0;

	return _rows.length();
}

int TableModelVariablesLevels::columnCount(const QModelIndex &parent) const
{
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
			return row.name();

		OptionList *list = row.option();
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
	else if (role == Qt::DecorationRole)
	{
		if (row.isLevel() == false)
		{
			switch (row.variable().second)
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
	}
	else if (role == Qt::ForegroundRole)
	{
		if (index.row() == _rows.length() - 1)
			return QBrush(QColor(0xAA, 0xAA, 0xAA));
	}
	else if (role == Qt::TextAlignmentRole)
	{
		if (row.isLevel())
			return Qt::AlignCenter;
		if (row.isOption())
			return Qt::AlignTop;
	}
	else if (role == Qt::SizeHintRole)
	{
		if (row.isLevel() || row.isOption())
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
		else if (row.isVariable())
			flags |= Qt::ItemIsSelectable | Qt::ItemIsDragEnabled;
	}

	return flags;
}

bool TableModelVariablesLevels::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (_boundTo == NULL)
		return false;

	OptionList *list = _rows.at(index.row()).option();

	QString qsValue = value.toString();
	QByteArray bytes = qsValue.toUtf8();

	list->setValue(string(bytes.constData(), bytes.length()));

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
		dataStream << row.variable();
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
		QDataStream stream(&encodedData, QIODevice::ReadOnly);

		int count;

		stream >> count;

		if (count == 0)
			return false;

		if (parent.isValid() == false)
		{
			if (row == -1)
				row = _rows.length();
			else if (row == 0)
				row = 1;

			int level = 0;
			int positionInLevel = 0;
			for (int i = 1; i < row; i++)
			{
				if (_rows[i].isLevel())
				{
					level++;
					positionInLevel = 0;
				}
				else if (_rows[i].isOption() == false)
				{
					positionInLevel++;
				}
			}

			if (level == _boundTo->size())
			{
				Options *options = dynamic_cast<Options*>(_boundTo->rowTemplate()->clone());
				OptionString *nameOption = dynamic_cast<OptionString *>(options->get("name"));
				string levelName = fq(tq(nameOption->value()).arg(level + 1));
				nameOption->setValue(levelName);
				_boundTo->insert(level, options);
			}

			Options *levelOption = _boundTo->at(level);
			OptionFields *variablesOption = dynamic_cast<OptionFields*>(levelOption->get("variables"));
			vector<string> variables = variablesOption->value();

			vector<string>::iterator insertionPoint = variables.begin();
			for (int i = 0; i < positionInLevel; i++)
				insertionPoint++;
			variables.insert(insertionPoint, count, "");

			for (int i = 0; i < count && !stream.atEnd(); i++)
			{
				ColumnInfo item;
				stream >> item;

				variables[positionInLevel + i] = fq(item.first);
			}

			variablesOption->setValue(variables);

			readFromOption();

		}

		return true;
	}

	return false;
}

bool TableModelVariablesLevels::canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const
{
	if (mimeTypes().contains("application/vnd.list.variable"))
	{
		QByteArray encodedData = data->data("application/vnd.list.variable");
		QDataStream stream(&encodedData, QIODevice::ReadOnly);

		if (stream.atEnd()) // is empty
			return false;

		int count;

		stream >> count;

		if (count >= 1)
			return true;
	}

	return false;
}

void TableModelVariablesLevels::setSource(ListModelVariablesAvailable *source)
{
	_source = source;
}

void TableModelVariablesLevels::readFromOption()
{
	beginResetModel();

	_rows.clear();

	if (_boundTo != NULL)
	{
		int i;

		for (i = 0; i < _boundTo->size(); i++)
		{
			Options *level = _boundTo->at(i);
			OptionFields *variablesOption = dynamic_cast<OptionFields *>(level->get("variables"));
			OptionString *nameOption = dynamic_cast<OptionString *>(level->get("name"));
			vector<string> variables = variablesOption->value();

			_rows.append(Row(tq(nameOption->value())));

			for (int j = 2; j < level->size(); j++)
			{
				OptionList *list = dynamic_cast<OptionList *>(level->get(j));
				_rows.append(Row(list));
			}

			QList<ColumnInfo> vars = _source->retrieveInfo(variables);

			foreach (const ColumnInfo &variable, vars)
				_rows.append(Row(variable));
		}

		OptionString *nameTemplate = static_cast<OptionString *>(_boundTo->rowTemplate()->get("name"));
		QString name = tq(nameTemplate->value()).arg(i + 1);
		_rows.append(Row(name));
	}

	endResetModel();
}

