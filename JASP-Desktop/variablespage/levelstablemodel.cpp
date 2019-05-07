
#include "levelstablemodel.h"

#include <boost/container/vector.hpp>
#include <algorithm>

#include <QColor>

#include "utilities/qutils.h"

LevelsTableModel::LevelsTableModel(QObject *parent)
	: QAbstractTableModel(parent)
{
	_column = NULL;
	connect(this, &LevelsTableModel::refreshConnectedModels, this, &LevelsTableModel::refreshConnectedModelsToName);
}

void LevelsTableModel::setColumn(Column *column)
{
	beginResetModel();
	_column = column;
	_colName = column != NULL ? column->name() : "";
	endResetModel();
	emit resizeLabelColumn();

	if(column == NULL)	setChosenColumn(-1);
	else				setChosenColumn(_dataSet->getColumnIndex(_colName));
}

void LevelsTableModel::refreshColumn(Column * column)
{
	if(_column != NULL && _column == column)
		refresh();
}

void LevelsTableModel::refresh()
{
	beginResetModel();
	endResetModel();
	emit resizeLabelColumn();
	emit filteredOutChanged();
}

void LevelsTableModel::clearColumn()
{
	setColumn(NULL);
}

void LevelsTableModel::setDataSet(DataSet * thisDataSet)
{
	_dataSet = thisDataSet;

	int newIndex = _dataSet == NULL ? -1 : _dataSet->getColumnIndex(_colName);

	if(newIndex == -1)	clearColumn(); //also resets model
	else				setColumn(&(_dataSet->column(newIndex)));
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

	return 3;
}

QVariant LevelsTableModel::data(const QModelIndex &index, int role) const
{
	if (role == Qt::BackgroundColorRole && index.column() == 0)
		return QColor(0xf6,0xf6,0xf6);

	// (role != Qt::DisplayRole && role != Qt::EditRole)
	//	return QVariant();

	Labels &labels = _column->labels();
	int row = index.row();

	if(row < 0 || row >= rowCount())
		return QVariant();


	if(role == (int)Roles::ValueRole) return tq(labels.getValueFromRow(row));
	if(role == (int)Roles::LabelRole) return tq(labels.getLabelFromRow(row));
	if(role == (int)Roles::FilterRole) return QVariant(labels[row].filterAllows());

	if(role == Qt::DisplayRole)
	{
		if (index.column() == 0)
			return tq(labels.getValueFromRow(row));
		else if(index.column() == 1)
			return tq(labels.getLabelFromRow(row));
		else if(index.column() == 2)
			return QVariant(labels[row].filterAllows());
	}

	return QVariant();
}

QVariant LevelsTableModel::headerData(int section, Qt::Orientation orientation, int role) const
{
	if(role == (int)Roles::ValueRole) return "Value";
	if(role == (int)Roles::LabelRole) return "Label";
	if(role == (int)Roles::FilterRole) return "Filter";

	if (role != Qt::DisplayRole)
		return QVariant();

	if (orientation != Qt::Horizontal)
		return QVariant();

	if (section == 0)
		return "Value";
	else
		return "Label";
}

void LevelsTableModel::_moveRows(QModelIndexList &selection, bool up) {
	if (_column == NULL)
		return;


	Labels &labels = _column->labels();
	std::vector<Label> new_labels(labels.begin(), labels.end());

	for (QModelIndex &index : selection)
	{
		//if (index.column() == 0) {
			iter_swap(new_labels.begin() + index.row(), new_labels.begin() + (index.row() + (up ? - 1: 1)));
		//}
	}

	beginResetModel();
	labels.set(new_labels);
	endResetModel();

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

	beginResetModel();

    Labels &labels = _column->labels();
	std::vector<Label> new_labels(labels.begin(), labels.end());

    std::reverse(new_labels.begin(), new_labels.end());

    labels.set(new_labels);

	/*QModelIndex topLeft = createIndex(0,0);
    QModelIndex bottonRight = createIndex(labels.size() - 1, 1);
    //emit a signal to make the view reread identified data
	emit dataChanged(topLeft, bottonRight);*/
	endResetModel();
}

Qt::ItemFlags LevelsTableModel::flags(const QModelIndex &index) const
{
	if (index.column() == 1) {
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
			if (labels.setLabelFromRow(index.row(), new_label))
			{
				emit dataChanged(index, index);

				emit refreshConnectedModels(_column);

				emit labelFilterChanged();
			}

		}
    }

    return true;
}


QHash<int, QByteArray> LevelsTableModel::roleNames() const
{
	static const QHash<int, QByteArray> roles = QHash<int, QByteArray> { {(int)Roles::ValueRole, "value"}, {(int)Roles::LabelRole, "label"}, {(int)Roles::FilterRole, "filter"} };
	return roles;
}


QModelIndexList LevelsTableModel::convertQVariantList_to_QModelIndexList(QVariantList selection)
{
	QModelIndexList List;
	bool Converted;

	for(QVariant variant : selection)
	{
		int Row = variant.toInt(&Converted);
		if(Converted)
			List << index(Row, 0);
	}

	return List;

}

int LevelsTableModel::currentColumnIndex()
{
	if(_column == NULL)
		return -1;

	try			{ return _dataSet->columns().findIndexByName(_column->name()); }
	catch(...)	{ return -1; }
}

void LevelsTableModel::resetFilterAllows()
{
	beginResetModel();
	_column->resetFilter();
	endResetModel();

	emit notifyColumnHasFilterChanged(currentColumnIndex());
	emit labelFilterChanged();
	emit filteredOutChanged();
}

bool LevelsTableModel::setAllowFilterOnLabel(int row, bool newAllowValue)
{
	bool atLeastOneRemains = newAllowValue;

	if(!atLeastOneRemains) //Do not let the user uncheck every single one because that is useless, the user wants to uncheck row so lets see if there is another one left after that.
		for(size_t i=0; i< _column->labels().size(); i++)
			if(i != row && _column->labels()[i].filterAllows())
			{
				atLeastOneRemains = true;
				break;
			}

	if(atLeastOneRemains)
	{
		bool before = _column->hasFilter();
		_column->labels()[row].setFilterAllows(newAllowValue);
		if(before != _column->hasFilter())
			emit notifyColumnHasFilterChanged(currentColumnIndex());

		emit labelFilterChanged();
		emit dataChanged(index(row, 2), index(row, 2)); //to make sure the checkbox is set to the right value
		emit filteredOutChanged();
	}

	return atLeastOneRemains;
}

bool LevelsTableModel::allowFilter(int row)
{
	return _column->labels()[row].filterAllows();
}

int LevelsTableModel::filteredOut()
{
	if(_column == NULL)
		return 0;

	int filteredOut = 0;

	for(size_t i=0; i< _column->labels().size(); i++)
		if(!_column->labels()[i].filterAllows())
			filteredOut++;

	return filteredOut;
}


void LevelsTableModel::setChosenColumn(int chosenColumn)
{
	if (_chosenColumn == chosenColumn)
		return;

	_chosenColumn = chosenColumn;
	emit chosenColumnChanged(_chosenColumn);
}
