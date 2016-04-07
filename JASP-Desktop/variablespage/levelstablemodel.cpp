
#include "levelstablemodel.h"
#include <boost/foreach.hpp>
#include <boost/container/vector.hpp>
#include <algorithm>
#include <QDebug>

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

void LevelsTableModel::_moveRows(QModelIndexList &selection, bool up) {
	if (_column == NULL)
		return;

	Labels &labels = _column->labels();
	std::vector<LabelEntry> new_labels(labels.begin(), labels.end());

	BOOST_FOREACH (QModelIndex &index, selection)
	{
		if (index.column() == 0) {
			iter_swap(new_labels.begin() + index.row(), new_labels.begin() + (index.row() + (up ? - 1: 1)));
		}
	}
	labels.set(new_labels);

	QModelIndex topLeft = createIndex(0,0);
	QModelIndex bottonRight = createIndex(labels.size() - 1, 1);
	//emit a signal to make the view reread identified data
	emit dataChanged(topLeft, bottonRight);
}

void LevelsTableModel::moveUp(QModelIndexList &selection) {
	_moveRows(selection, true);
}

void LevelsTableModel::moveDown(QModelIndexList &selection) {
	_moveRows(selection, false);
}

void LevelsTableModel::reverse() {
    if (_column == NULL)
        return;

    Labels &labels = _column->labels();
    std::vector<LabelEntry> new_labels(labels.begin(), labels.end());

    std::reverse(new_labels.begin(), new_labels.end());

    labels.set(new_labels);

    QModelIndex topLeft = createIndex(0,0);
    QModelIndex bottonRight = createIndex(labels.size() - 1, 1);
    //emit a signal to make the view reread identified data
    emit dataChanged(topLeft, bottonRight);
}

Qt::ItemFlags LevelsTableModel::flags(const QModelIndex &index) const
{
    if (index.column() == 0) {
        return Qt::ItemIsEditable | QAbstractTableModel::flags(index);
    } else {
        return QAbstractTableModel::flags(index);
    }
}

bool LevelsTableModel::setData(const QModelIndex & index, const QVariant & value, int role)
{
    if (_column == NULL)
        return false;

    if (role == Qt::EditRole)
    {
        const std::string &new_label = value.toString().toStdString();
        if (new_label != "") {
            Labels &labels = _column->labels();
            labels.setLabel(index.row(), new_label);
        }
    }
    return true;
}
