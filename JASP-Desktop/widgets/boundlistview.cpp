#include "boundlistview.h"

#include "boost/bind.hpp"
#include "boost/foreach.hpp"

#include <vector>
#include <string>

#include "options/optionfield.h"

using namespace std;

BoundListView::BoundListView(QWidget *parent)
	: QListView(parent),
	  _listModel(this)
{
	_availableFieldsListView = NULL;
	_assignButton = NULL;

	setEditTriggers(QListView::NoEditTriggers);
	setModel(&_listModel);
}

void BoundListView::setDataSet(DataSet *dataSet)
{
	_listModel.setDataSet(dataSet);
}

void BoundListView::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionFields *>(option);

	if (_boundTo != NULL)
	{
		_boundTo->changed.connect(boost::bind(&BoundListView::updateList, this));
		updateList();
	}
	else
		qDebug() << "could not bind to OptionAssignedFields in BoundListView.cpp";
}

void BoundListView::setAssignButton(AssignButton *button)
{
	_assignButton = button;

	if (button != NULL)
		connect(button, SIGNAL(clicked()), this, SLOT(assign()));
}

void BoundListView::setAvailableFieldsListView(AvailableFieldsListView *listView)
{
	_availableFieldsListView = listView;
}

void BoundListView::focusInEvent(QFocusEvent *event)
{
	if (selectedIndexes().length() > 0 && _assignButton != NULL)
		_assignButton->setAssignDirection(false);

	QListView::focusInEvent(event);
}

void BoundListView::selectionChanged(const QItemSelection &selected, const QItemSelection &deselected)
{
	if (_assignButton != NULL)
		_assignButton->setAssignDirection(false);

	QListView::selectionChanged(selected, deselected);
}


void BoundListView::assign()
{
	if (_boundTo != NULL)
	{
		if (_assignButton->isAssign())
		{
			if (_availableFieldsListView == NULL)
				return;

			QStringList toAssign = _availableFieldsListView->selectedFields();

			vector<string> alreadyAssigned;
			if (dynamic_cast<OptionField *>(_boundTo) == NULL)
				alreadyAssigned = _boundTo->value();

			BOOST_FOREACH(QString &field, toAssign)
			{
				QByteArray utf8 = field.toUtf8();
				alreadyAssigned.push_back(string(utf8.constData(), utf8.length()));
			}

			_boundTo->setValue(alreadyAssigned);
		}
		else
		{
			QModelIndexList indices = this->selectedIndexes();

			vector<string> currentFields = _boundTo->value();
			vector<string> toRetain = currentFields;

			BOOST_FOREACH(QModelIndex &index, indices)
			{
				string toRemove = currentFields.at(index.row());

				vector<string>::iterator itr = toRetain.begin();

				while (itr != toRetain.end())
				{
					if (*itr == toRemove)
					{
						toRetain.erase(itr);
						break;
					}
					itr++;
				}
			}

			_boundTo->setValue(toRetain);
		}
	}

}

void BoundListView::updateList()
{
	QStringList list;

	BOOST_FOREACH(string value, _boundTo->value())
	{
		QString asString = QString::fromUtf8(value.c_str(), value.length());
		list.append(asString);
	}

	_listModel.setAssigned(list);
}

BoundListView::AssignedVariables::AssignedVariables(QObject *parent) :
	QAbstractListModel(parent)
{
	_dataSet = NULL;

	_nominalIcon = QIcon(":/icons/variable-nominal.png");
	_ordinalIcon = QIcon(":/icons/variable-ordinal.png");
	_scaleIcon = QIcon(":/icons/variable-scale.png");
}

void BoundListView::AssignedVariables::setDataSet(DataSet *dataSet)
{
	_dataSet = dataSet;
}

int BoundListView::AssignedVariables::rowCount(const QModelIndex &) const
{
	return _assignedVariables.length();
}

QVariant BoundListView::AssignedVariables::data(const QModelIndex &index, int role) const
{
	int row = index.row();

	if (role == Qt::DisplayRole)
	{
		return QVariant(_assignedVariables.at(row));
	}
	else if (role == Qt::DecorationRole)
	{
		QString variable = _assignedVariables.at(row);
		QByteArray utf8 = variable.toUtf8();
		string n(utf8.constData(), utf8.length());
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
	else
	{
		return QVariant();
	}
}

QStringList BoundListView::AssignedVariables::assigned()
{
	return _assignedVariables;
}

void BoundListView::AssignedVariables::setAssigned(QStringList assigned)
{
	beginResetModel();
	_assignedVariables = assigned;
	endResetModel();
}


