#include "boundlistview.h"

#include "boost/bind.hpp"
#include "boost/foreach.hpp"

#include <vector>
#include <string>

#include "options/optionfield.h"

using namespace std;

BoundListView::BoundListView(QWidget *parent)
	: QListView(parent)
{
	_availableFieldsListView = NULL;
	_assignButton = NULL;

	setEditTriggers(QListView::NoEditTriggers);
	setModel(&_listModel);
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
				itr->find(toRemove);
				toRetain.erase(itr);
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

	_listModel.setStringList(list);
}
