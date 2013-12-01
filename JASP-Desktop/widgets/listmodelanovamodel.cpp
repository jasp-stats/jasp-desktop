
#include "listmodelanovamodel.h"

#include <QMimeData>
#include <QDebug>

using namespace std;

ListModelAnovaModel::ListModelAnovaModel(QObject *parent)
	: TableModel(parent)
{
	_boundTo = NULL;
	_customModel = false;
	_dependent = ColumnInfo("", 0);
}

QVariant ListModelAnovaModel::data(const QModelIndex &index, int role) const
{
	if (index.isValid() == false)
		return QVariant();

	if (role == Qt::DisplayRole)
	{
		const QList<ColumnInfo> &interaction = _terms.at(index.row());
		QString asString;
		bool first = true;

		foreach (ColumnInfo variable, interaction)
		{
			if (first)
			{
				first = false;
				asString = variable.first;
			}
			else
			{
				asString += " : " + variable.first;
			}
		}

		return QVariant(asString);
	}

	return QVariant();
}

int ListModelAnovaModel::rowCount(const QModelIndex &) const
{
	return _terms.length();
}

int ListModelAnovaModel::columnCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);

	return 1;
}

QStringList ListModelAnovaModel::mimeTypes() const
{
	QStringList types;

	types << "application/vnd.list.term";

	return types;
}

void ListModelAnovaModel::setVariables(const QList<ColumnInfo> &variables)
{
	_variables = variables;

	int row = 0;

	foreach (const QList<ColumnInfo> &term, _terms)
	{
		bool shouldRemove = false;

		foreach (const ColumnInfo &item, term)
		{
			if (variables.contains(item) == false)
			{
				shouldRemove = true;
				break;
			}
		}

		if (shouldRemove)
			removeRow(row);
		else
			row++;
	}

	emit variablesAvailableChanged();

	assignToOption();
}

const QList<ColumnInfo> &ListModelAnovaModel::variables() const
{
	return _variables;
}

void ListModelAnovaModel::setDependent(const ColumnInfo dependent)
{
	_dependent = dependent;

	assignToOption();
}

void ListModelAnovaModel::setCustomModelMode(bool on)
{
	_customModel = on;

	assignToOption();
}

void ListModelAnovaModel::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionFields *>(option);

	beginResetModel();
	_variables.clear();
	endResetModel();
}

void ListModelAnovaModel::mimeDataMoved(const QModelIndexList &indexes)
{
	beginResetModel();

	QModelIndexList sorted = indexes;

	int lastRowDeleted = -1;

	qSort(sorted.begin(), sorted.end(), qGreater<QModelIndex>());

	foreach (const QModelIndex &index, sorted)
	{
		int row = index.row();
		if (row != lastRowDeleted)
			_terms.removeAt(row);
		lastRowDeleted = row;
	}

	endResetModel();

	assignToOption();
}

bool ListModelAnovaModel::canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const
{
	if (mimeTypes().contains("application/vnd.list.term"))
	{
		QByteArray encodedData = data->data("application/vnd.list.term");
		QDataStream stream(&encodedData, QIODevice::ReadOnly);

		if (stream.atEnd())
			return false;

		return true;
	}

	return false;
}

bool ListModelAnovaModel::dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent, int assignType)
{
	if (action == Qt::IgnoreAction)
		return true;

	if ( ! canDropMimeData(data, action, row, column, parent))
		return false;

	if (data->hasFormat("application/vnd.list.term"))
	{
		QByteArray encodedData = data->data("application/vnd.list.term");
		QDataStream stream(&encodedData, QIODevice::ReadOnly);
		QVector<ColumnInfo> items;

		if (stream.atEnd())
			return false;

		int count;
		stream >> count;

		while (!stream.atEnd())
		{
			ColumnInfo variable;
			stream >> variable;
			items << variable;
		}

		int itemCount = items.size();

		QList<QList<ColumnInfo> > terms;

		switch (assignType)
		{
		case Cross:
			terms = generateCrossCombinations(items);
			break;
		case Interaction:
			terms = generateWayCombinations(items, itemCount);
			break;
		case MainEffects:
			terms = generateWayCombinations(items, 1);
			break;
		case All2Way:
			terms = generateWayCombinations(items, 2);
			break;
		case All3Way:
			terms = generateWayCombinations(items, 3);
			break;
		case All4Way:
			terms = generateWayCombinations(items, 4);
			break;
		case All5Way:
			terms = generateWayCombinations(items, 5);
			break;
		}

		beginResetModel();

		foreach (const QList<ColumnInfo> term, terms)
		{
			int r = 0;
			bool added = false;
			foreach (const QList<ColumnInfo> &existingInteraction, _terms)
			{
				if (term.length() == existingInteraction.length())
				{
					bool match = true;

					foreach (const ColumnInfo &variable, existingInteraction)
					{
						if ( ! term.contains(variable))
						{
							match = false;
							break;
						}
					}

					if (match)
					{
						_terms[r] = term;
						added = true;
					}

				}

				r++;
			}

			if ( ! added)
			{
				r = rowCount(QModelIndex());
				insertRow(r);
				_terms[r] = term;
				//_terms.append(term);
			}
		}

		endResetModel();

		assignToOption();

		return true;
	}

	return false;
}

bool ListModelAnovaModel::dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent)
{
	return dropMimeData(data, action, row, column, parent, Cross);
}

QMimeData *ListModelAnovaModel::mimeData(const QModelIndexList &indexes) const
{
	/* returns dummy data. when the user drags entries away from this listbox
	 * it means that they're deleting entries, so there's no point populating
	 * this data object properly
	 */

	QMimeData *mimeData = new QMimeData();
	QByteArray encodedData;

	QDataStream dataStream(&encodedData, QIODevice::WriteOnly);

	dataStream << 0;

	mimeData->setData("application/vnd.list.term", encodedData);

	return mimeData;
}

bool ListModelAnovaModel::insertRows(int row, int count, const QModelIndex &parent)
{
	beginInsertRows(parent, row, row + count - 1);

	for (int i = 0; i < count; i++)
		_terms.insert(row, QList<ColumnInfo>());

	endInsertRows();

	return true;
}

Qt::ItemFlags ListModelAnovaModel::flags(const QModelIndex &index) const
{
	if (index.isValid())
		return Qt::ItemIsEnabled | Qt::ItemIsSelectable | Qt::ItemIsDragEnabled;
	else
		return Qt::ItemIsEnabled | Qt::ItemIsDropEnabled;
}

QVariant ListModelAnovaModel::headerData(int section, Qt::Orientation orientation, int role) const
{
	if (role == Qt::DisplayRole && orientation == Qt::Horizontal && section == 0)
		return "Model Terms";

	return QVariant();
}

Qt::DropActions ListModelAnovaModel::supportedDropActions() const
{
	return Qt::CopyAction;
}

Qt::DropActions ListModelAnovaModel::supportedDragActions() const
{
	return Qt::MoveAction;
}

QList<QList<ColumnInfo> > ListModelAnovaModel::generateCrossCombinations(const QVector<ColumnInfo> &variables)
{
	QList<QList<ColumnInfo> > combinations;

	for (int r = 1; r <= variables.size(); r++)
	{
		std::vector<bool> v(variables.size());
		std::fill(v.begin() + r, v.end(), true);

		do {

			QList<ColumnInfo> combination;

			for (int i = 0; i < variables.size(); ++i) {
				if (!v[i])
					combination << variables.at(i);
			}

			combinations << combination;

		} while (std::next_permutation(v.begin(), v.end()));
	}

	return combinations;
}

QList<QList<ColumnInfo> > ListModelAnovaModel::generateWayCombinations(const QVector<ColumnInfo> &variables, int ways)
{
	QList<QList<ColumnInfo> > combinations;

	for (int r = ways; r <= ways; r++)
	{
		std::vector<bool> v(variables.size());
		std::fill(v.begin() + r, v.end(), true);

		do {

			QList<ColumnInfo> combination;

			for (int i = 0; i < variables.size(); ++i) {
				if (!v[i])
					combination << variables.at(i);
			}

			combinations << combination;

		} while (std::next_permutation(v.begin(), v.end()));
	}

	return combinations;
}

QString ListModelAnovaModel::itemsToString(QList<ColumnInfo> items)
{
	QString result;

	bool firstItem = true;

	foreach (const ColumnInfo &item, items)
	{
		if (firstItem)
			firstItem = false;
		else
			result += ":";

		result += item.first;
	}

	return result;
}

void ListModelAnovaModel::assignToOption()
{
	if (_boundTo == NULL)
		return;

	vector<string> values;

	if (_customModel)
	{
		foreach (QList<ColumnInfo> term, _terms)
		{
			QString modelTerm = itemsToString(term);
			QByteArray utf8 = modelTerm.toUtf8();
			string modelTermAsString(utf8.constData(), utf8.length());
			values.push_back(modelTermAsString);
		}
	}
	else
	{
		QList<QList<ColumnInfo> > combinations = generateCrossCombinations(_variables.toVector());

		foreach (QList<ColumnInfo> term, combinations)
		{
			QString modelTerm = itemsToString(term);
			QByteArray utf8 = modelTerm.toUtf8();
			string modelTermAsString(utf8.constData(), utf8.length());
			values.push_back(modelTermAsString);
		}
	}

	_boundTo->setValue(values);

}
