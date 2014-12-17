
#include "tablemodelanovamodel.h"

#include <QMimeData>
#include <QDebug>

#include "utils.h"
#include <boost/foreach.hpp>

#include "options/optionboolean.h"

using namespace std;

TableModelAnovaModel::TableModelAnovaModel(QObject *parent)
	: TableModel(parent)
{
	_boundTo = NULL;
	_customModel = false;

	_terms.setSortParent(_variables);
}

QVariant TableModelAnovaModel::data(const QModelIndex &index, int role) const
{
	if (_boundTo == NULL || index.isValid() == false)
		return QVariant();

	if (role == Qt::DisplayRole)
	{
		int colNo = index.column();
		int rowNo = index.row();
		Options *row = _rows.at(rowNo);

		if (colNo == 0) {

			OptionTerms *termOption = static_cast<OptionTerms *>(row->get(0));
			Term t(termOption->value().front());
			return t.asQString();
		}
	}
	else if (role == Qt::CheckStateRole)
	{
		int colNo = index.column();
		int rowNo = index.row();
		Options *row = _rows.at(rowNo);

		if (colNo == 1)
		{
			OptionBoolean *booleanOption = static_cast<OptionBoolean *>(row->get(1));
			return booleanOption->value() ? Qt::Checked : Qt::Unchecked;
		}
	}

	return QVariant();
}

int TableModelAnovaModel::rowCount(const QModelIndex &) const
{
	if (_boundTo == NULL)
		return 0;

	return _rows.size();
}

int TableModelAnovaModel::columnCount(const QModelIndex &) const
{
	if (_boundTo == NULL)
		return 0;

	return _boundTo->rowTemplate()->size();
}

bool TableModelAnovaModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (index.isValid() == false)
		return false;

	if (index.column() == 1 && role == Qt::CheckStateRole)
	{
		Options *row = _rows.at(index.row());
		OptionBoolean *booleanOption = static_cast<OptionBoolean *>(row->get(1));
		booleanOption->setValue(value.toInt() == Qt::Checked);

		_boundTo->setValue(_rows);

		return true;
	}

	return false;
}

QStringList TableModelAnovaModel::mimeTypes() const
{
	QStringList types;

	types << "application/vnd.list.term";

	return types;
}

void TableModelAnovaModel::setVariables(const Terms &variables)
{
	beginResetModel();

	_variables.set(variables);

	if (_customModel)
	{
		Terms terms = _terms;
		terms.discardWhatDoesntContainTheseComponents(_variables);
		if (terms.size() != _terms.size())
			setTerms(terms);
	}
	else
	{
		Terms terms = _variables.crossCombinations();
		if (terms != _terms)
			setTerms(terms);
	}

	emit variablesAvailableChanged();

	endResetModel();
}

const Terms &TableModelAnovaModel::variables() const
{
	return _variables;
}

void TableModelAnovaModel::setCustomModelMode(bool on)
{
	_customModel = on;

	if (_customModel)
		clear();
	else
		setTerms(_variables.crossCombinations());
}

void TableModelAnovaModel::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionsTable *>(option);
}

void TableModelAnovaModel::mimeDataMoved(const QModelIndexList &indexes)
{
	// sort indices, and delete from end to beginning

	QModelIndexList sorted = indexes;
	qSort(sorted.begin(), sorted.end(), qGreater<QModelIndex>());

	int lastRowDeleted = -1;

	Terms terms = _terms;

	foreach (const QModelIndex &index, sorted)
	{
		int rowNo = index.row();
		if (rowNo != lastRowDeleted)
			terms.remove(index.row());
		lastRowDeleted = rowNo;
	}

	setTerms(terms);
}

const Terms &TableModelAnovaModel::terms() const
{
	return _terms;
}

bool TableModelAnovaModel::canDropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent) const
{
	Q_UNUSED(action);
	Q_UNUSED(row);
	Q_UNUSED(column);
	Q_UNUSED(parent);

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

bool TableModelAnovaModel::dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent, int assignType)
{
	if (action == Qt::IgnoreAction)
		return true;

	if ( ! canDropMimeData(data, action, row, column, parent))
		return false;

	if ( ! data->hasFormat("application/vnd.list.term"))
		return false;


	QByteArray encodedData = data->data("application/vnd.list.term");

	Terms dropped;
	dropped.setSortParent(_variables);
	dropped.set(encodedData);

	Terms newTerms;

	switch (assignType)
	{
	case Cross:
		newTerms = dropped.crossCombinations();
		break;
	case Interaction:
		newTerms = dropped.wayCombinations(dropped.size());
		break;
	case MainEffects:
		newTerms = dropped.wayCombinations(1);
		break;
	case All2Way:
		newTerms = dropped.wayCombinations(2);
		break;
	case All3Way:
		newTerms = dropped.wayCombinations(3);
		break;
	case All4Way:
		newTerms = dropped.wayCombinations(4);
		break;
	case All5Way:
		newTerms = dropped.wayCombinations(5);
		break;
	default:
		(void)newTerms;
	}

	assign(newTerms);

	return true;
}

bool TableModelAnovaModel::dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int column, const QModelIndex &parent)
{
	return dropMimeData(data, action, row, column, parent, Cross);
}

QMimeData *TableModelAnovaModel::mimeData(const QModelIndexList &indexes) const
{
	/* returns dummy data. when the user drags entries away from this listbox
	 * it means that they're deleting entries, so there's no point populating
	 * this data object properly
	 */

	Q_UNUSED(indexes);

	QMimeData *mimeData = new QMimeData();
	QByteArray encodedData;

	QDataStream dataStream(&encodedData, QIODevice::WriteOnly);

	dataStream << 0;

	mimeData->setData("application/vnd.list.term", encodedData);

	return mimeData;
}

Qt::ItemFlags TableModelAnovaModel::flags(const QModelIndex &index) const
{
	if (index.isValid())
	{
		if (index.column() == 0)
			return Qt::ItemIsEnabled | Qt::ItemIsSelectable | Qt::ItemIsDragEnabled;
		else
			return Qt::ItemIsEnabled | Qt::ItemIsUserCheckable;
	}
	else
	{
		return Qt::ItemIsEnabled | Qt::ItemIsDropEnabled;
	}
}

QVariant TableModelAnovaModel::headerData(int section, Qt::Orientation orientation, int role) const
{	
	if (role == Qt::DisplayRole && orientation == Qt::Horizontal)
	{
		if (section == 0)
			return "Model Terms";
		if (section == 1)
			return "Is Nuisance";
	}

	return QVariant();
}

Qt::DropActions TableModelAnovaModel::supportedDropActions() const
{
	return Qt::CopyAction;
}

Qt::DropActions TableModelAnovaModel::supportedDragActions() const
{
	return Qt::MoveAction;
}

OptionVariables *TableModelAnovaModel::termOptionFromRow(Options *row)
{
	return static_cast<OptionVariables *>(row->get(0));
}

void TableModelAnovaModel::setTerms(const Terms &terms)
{
	_terms.set(terms);

	if (_boundTo == NULL)
		return;

	beginResetModel();

	BOOST_FOREACH(Options *row, _rows)
		delete row;

	_rows.clear();

	BOOST_FOREACH(const Term &term, _terms.terms())
	{
		(void)_terms;
		Options *row = static_cast<Options *>(_boundTo->rowTemplate()->clone());
		OptionTerms *termCell = static_cast<OptionTerms *>(row->get(0));
		termCell->setValue(term.scomponents());

		_rows.push_back(row);
	}

	_boundTo->setValue(_rows);

	endResetModel();

	emit termsChanged();
}

void TableModelAnovaModel::clear()
{
	setTerms(Terms());
}

void TableModelAnovaModel::assign(const Terms &terms)
{
	Terms t = _terms;
	t.add(terms);
	setTerms(t);
}
