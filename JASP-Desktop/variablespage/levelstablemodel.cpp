
#include "levelstablemodel.h"

#include "qutils.h"

LevelsTableModel::LevelsTableModel(QObject *parent)
	: QAbstractTableModel(parent)
{
	_column = NULL;
}

void LevelsTableModel::setColumn(Column *column)
{
	beginResetModel();
	_column = column;
	endResetModel();
}

void LevelsTableModel::clearColumn()
{
	setColumn(NULL);
}

int LevelsTableModel::rowCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);

	if (_column == NULL)
		return 0;

	return _column->labels().size();
}

int LevelsTableModel::columnCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);

	return 2;
}

QVariant LevelsTableModel::data(const QModelIndex &index, int role) const
{
	if (role != Qt::DisplayRole)
		return QVariant();

	Labels &labels = _column->labels();
	Labels::const_iterator itr = labels.begin();

	for (int i = 0; i < index.row(); i++)
		itr++;

	const LabelEntry &entry = *itr;

	if (index.column() == 0)
	{
		const Label &label = entry.second;
		return tq(label.text());
	}
	else
	{
		int raw = entry.first;
		return raw;
	}
}

QVariant LevelsTableModel::headerData(int section, Qt::Orientation orientation, int role) const
{
	if (role != Qt::DisplayRole)
		return QVariant();

	if (orientation != Qt::Horizontal)
		return QVariant();

	if (section == 0)
		return "Labels";
	else
		return "Value";
}

