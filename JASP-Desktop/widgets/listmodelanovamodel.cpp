
#include "listmodelanovamodel.h"

#include <QMimeData>
#include <QDebug>

ListModelAnovaModel::ListModelAnovaModel(QObject *parent)
	: QAbstractListModel(parent)
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

	bool reset = false;

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
		{
			if (reset == false)
			{
				beginResetModel();
				reset = true;
			}

			_terms.removeOne(term);
		}
	}

	if (reset)
	{
		endResetModel();
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
	_boundTo = dynamic_cast<OptionString *>(option);
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
			int r = -1;
			bool added = false;
			foreach (const QList<ColumnInfo> &existingInteraction, _terms)
			{
				r++;

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
			}


			if ( ! added)
				_terms.append(term);
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

bool ListModelAnovaModel::removeRows(int row, int count, const QModelIndex &parent)
{
	beginRemoveRows(parent, row, row + count - 1);

	for (int i = 0; i < count; i++)
		_terms.removeAt(row);

	endRemoveRows();

	assignToOption();
}

Qt::ItemFlags ListModelAnovaModel::flags(const QModelIndex &index) const
{
	if (index.isValid())
		return Qt::ItemIsEnabled | Qt::ItemIsSelectable | Qt::ItemNeverHasChildren | Qt::ItemIsDragEnabled;
	else
		return Qt::ItemIsEnabled | Qt::ItemIsDropEnabled;
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

QString ListModelAnovaModel::termsToString(QList<QList<ColumnInfo> > terms)
{
	QString result;

	bool first = true;

	foreach (const QList<ColumnInfo> &term, terms)
	{
		if (first)
			first = false;
		else
			result += " + ";

		bool firstItem = true;

		foreach (const ColumnInfo &item, term)
		{
			if (firstItem)
				firstItem = false;
			else
				result += ":";

			result += item.first;
		}
	}

	return result;
}

void ListModelAnovaModel::assignToOption()
{
	if (_boundTo == NULL)
		return;

	QString newModelDesc;

	if (_dependent.first != "" && _customModel && _terms.length() > 0)
	{
		newModelDesc = _dependent.first + " ~ " + termsToString(_terms);
	}
	else if (_dependent.first != "" && _customModel == false)
	{
		newModelDesc = _dependent.first + " ~ " + termsToString(generateCrossCombinations(_variables.toVector()));
	}

	if (_modelDesc != newModelDesc)
	{
		_modelDesc = newModelDesc;
		QByteArray utf8 = _modelDesc.toUtf8();
		std::string value(utf8.constData(), utf8.length());
		_boundTo->setValue(value);
	}

	qDebug() << _modelDesc;
}
