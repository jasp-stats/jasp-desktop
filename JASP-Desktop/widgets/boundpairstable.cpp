#include "boundpairstable.h"
#include "QDebug"
#include <QHeaderView>

#include <boost/foreach.hpp>

using namespace std;

BoundPairsTable::BoundPairsTable(QWidget *parent) :
	QTableView(parent),
	_model(parent)
{
	_availableFieldsListView = NULL;
	_assignButton = NULL;
	_boundTo = NULL;

	setModel(&_model);
	setSelectionMode(QAbstractItemView::ContiguousSelection);

	horizontalHeader()->setSectionResizeMode(0, QHeaderView::Stretch);
	horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
	horizontalHeader()->setSectionsClickable(false);
	horizontalHeader()->hide();

	verticalHeader()->hide();
}

void BoundPairsTable::bindTo(Option *option)
{	
	OptionFieldPairs *fieldPairs = dynamic_cast<OptionFieldPairs * >(option);

	if (fieldPairs != NULL)
	{
		_model.bindTo(fieldPairs);
		_boundTo = fieldPairs;
	}
	else
	{
		_boundTo = NULL;
		qDebug() << "BoundPairsTable::bindTo() could not cast Option* to OptionFieldPairs*";
	}
}

void BoundPairsTable::assign()
{
	if (_boundTo != NULL)
	{
		if (_assignButton->isAssign())
		{
			if (_availableFieldsListView == NULL)
				return;

			QStringList toAssign = _availableFieldsListView->selectedFields();

			if (toAssign.length() == 1)
			{
				// if assigning a single item

				QByteArray utf8 = toAssign[0].toUtf8();
				string v(utf8.constData(), utf8.length());

				vector<pair<string, string> > value = _boundTo->value();

				QModelIndexList indices = selectedIndexes();

				if (indices.length() > 0)
				{
					// if there's a selection, place it in the selection

					QModelIndex index = indices[0];

					size_t row = index.row();

					if (row < value.size())
					{
						pair<string, string> p = value.at(index.row());

						if (index.column() == 0)
							p.first = v;
						else
							p.second = v;

						value[index.row()] = p;
					}
					else
					{
						pair<string, string> p;

						if (index.column() == 0)
							p.first = v;
						else
							p.second = v;

						value.push_back(p);
					}

					_boundTo->setValue(value);
				}
				else
				{
					// if no selection, fill in a gap, or add at the end

					bool append = true;

					for (size_t i = 0; i < value.size(); i++)
					{
						pair<string, string> p = value[i];
						if (p.first == "")
						{
							p.first = v;
							value[i] = p;
							append = false;
							break;
						}
						if (p.second == "")
						{
							p.second = v;
							value[i] = p;
							append = false;
							break;
						}
					}

					if (append)
					{
						pair<string, string> p;
						p.first = v;
						value.push_back(p);
					}

					_boundTo->setValue(value);
				}
			}
			else if (toAssign.length() > 1)
			{
				// if assigning multiple items, assign the first two as a pair

				QByteArray utf8;

				utf8 = toAssign[0].toUtf8();
				string v1(utf8.constData(), utf8.length());

				utf8 = toAssign[1].toUtf8();
				string v2(utf8.constData(), utf8.length());

				pair<string, string> p(v1, v2);

				vector<pair<string, string> > value = _boundTo->value();
				value.push_back(p);
				_boundTo->setValue(value);
			}
		}
		else
		{
			// removing

			QModelIndexList indices = selectedIndexes();

			if (indices.length() == 1)
			{
				// remove single item

				FieldPairs pairs = _boundTo->value();

				QModelIndex index = indices.at(0);
				if (index.row() < pairs.size())
				{
					FieldPair p = pairs.at(index.row());

					if ((index.column() == 0 && p.second == "") || (index.column() == 1 && p.first == ""))
					{
						// remove pair if empty

						FieldPairs::iterator itr = pairs.begin();
						int i;
						for (i = 0; i < index.row(); i++)
							itr++;
						pairs.erase(itr);

						_boundTo->setValue(pairs);
					}
					else
					{
						// remove just the selected one

						if (index.column() == 0)
							p.first = "";
						else
							p.second = "";

						pairs[index.row()] = p;

						_boundTo->setValue(pairs);

						if (_assignButton != NULL)
							_assignButton->setAssignDirection(true);
					}
				}
			}
			else if (indices.length() > 1)
			{
				// remove whole pairs

				FieldPairs pairs = _boundTo->value();

				int firstRow = indices.first().row();

				if (firstRow != pairs.size())
				{
					int lastRow = indices.last().row();
					if (lastRow == pairs.size())
						lastRow--;

					FieldPairs::iterator begin = pairs.begin();

					int i;
					for (i = 0; i < firstRow; i++)
						begin++;

					FieldPairs::iterator end = begin;

					for (; i < lastRow; i++)
						end++;
					end++;

					pairs.erase(begin, end);

					_boundTo->setValue(pairs);
				}
			}
		}
	}
}

void BoundPairsTable::setAssignButton(AssignButton *button)
{
	_assignButton = button;

	if (button != NULL)
		connect(button, SIGNAL(clicked()), this, SLOT(assign()));
}

void BoundPairsTable::setDataSet(DataSet *dataSet)
{
	_model.setDataSet(dataSet);
}

void BoundPairsTable::selectionChanged(const QItemSelection &selected, const QItemSelection &deselected)
{
	QTableView::selectionChanged(selected, deselected);

	QModelIndexList indices = selectedIndexes();

	if (indices.length() == 1)
	{
		if (_assignButton != NULL)
			_assignButton->setAssignDirection(true);
	}
	else if (indices.length() > 1)
	{
		QModelIndex first = indices.first();
		QModelIndex last = indices.first();

		BOOST_FOREACH(QModelIndex &index, indices)
		{
			if (index.row() <= first.row() && index.column() <= first.column())
				first = index;
			if (index.row() >= last.row() && index.column() >= last.column())
				last = index;
		}

		int rowsSelected = last.row() - first.row() + 1;
		int colsSelected = last.column() - first.column() + 1;

		if (rowsSelected > 1 && colsSelected < 2)
		{
			first = _model.index(first.row(), 0);
			last = _model.index(last.row(), 1);

			QItemSelection selection;
			selection.select(first, last);

			this->selectionModel()->select(selection, QItemSelectionModel::ClearAndSelect);
		}

		if (_assignButton != NULL)
			_assignButton->setAssignDirection(false);
	}
	else // indicies.length == 0
	{
		if (_assignButton != NULL)
			_assignButton->setAssignDirection(true);
	}


}

void BoundPairsTable::setAvailableFieldsListView(AvailableFieldsListView *listView)
{
	_availableFieldsListView = listView;
}

BoundPairsTable::TableModel::TableModel(QWidget *parent)
	: QAbstractTableModel(parent)
{
	_dataSet = NULL;
	_boundTo = NULL;

	_nominalIcon = QIcon(":/icons/variable-nominal.png");
	_ordinalIcon = QIcon(":/icons/variable-ordinal.png");
	_scaleIcon = QIcon(":/icons/variable-scale.png");
}

void BoundPairsTable::TableModel::bindTo(OptionFieldPairs *option)
{
	beginResetModel();
	_boundTo = option;
	_value = option->value();
	_boundTo->changed.connect(boost::bind(&BoundPairsTable::TableModel::pairsChanged, this));
	endResetModel();
}

int BoundPairsTable::TableModel::rowCount(const QModelIndex &parent) const
{
	if (_boundTo == NULL)
		return 0;

	if (_value.size() == 0)
		return 1;
	else if (_value.rbegin()->second == "")
		return _value.size();
	else
		return _value.size() + 1;

}

int BoundPairsTable::TableModel::columnCount(const QModelIndex &parent) const
{
	return 2;
}

QVariant BoundPairsTable::TableModel::data(const QModelIndex &index, int role) const
{
	if (_boundTo == NULL)
		return QVariant();

	if (role == Qt::DisplayRole)
	{
		if (index.row() < _value.size())
		{
			string v;
			if (index.column() == 0)
				v = _value.at(index.row()).first;
			else
				v = _value.at(index.row()).second;

			return QString::fromUtf8(v.c_str(), v.size());
		}

		return QVariant("");
	}
	else if (role == Qt::DecorationRole)
	{
		if (index.row() >= _value.size())
			return QVariant();

		FieldPair p = _value.at(index.row());
		string n = (index.column() ? p.second : p.first);

		if (n == "")
			return QVariant();

		Column *column = _dataSet->columns().get(n);

		switch (column->columnType())
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

	return QVariant();
}

Qt::ItemFlags BoundPairsTable::TableModel::flags(const QModelIndex &index) const
{
	return Qt::ItemIsSelectable | Qt::ItemIsEnabled;
}

void BoundPairsTable::TableModel::setDataSet(DataSet *dataSet)
{
	_dataSet = dataSet;
}

void BoundPairsTable::TableModel::pairsChanged()
{
	beginResetModel();
	_value = _boundTo->value();
	endResetModel();
}

