#include "availablefieldslistview.h"

#include "boost/foreach.hpp"

#include "availablefields.h"

AvailableFieldsListView::AvailableFieldsListView(QWidget *parent) :
	QListView(parent)
{
	setEditTriggers(QListView::NoEditTriggers);
}

void AvailableFieldsListView::addAssignButton(AssignButton *button)
{
	_assignButtons.push_back(button);
}

QStringList AvailableFieldsListView::selectedFields() const
{
	QStringList fields;

	AvailableFields *availableFields = dynamic_cast<AvailableFields *>(this->model());
	if (availableFields == NULL)
		return fields;

	QStringList availableFieldsList = availableFields->stringList();
	QModelIndexList selected = selectionModel()->selectedRows();

	BOOST_FOREACH(const QModelIndex &index, selected)
	{
		QString field = availableFieldsList.at(index.row());
		fields.append(field);
	}

	return fields;
}

void AvailableFieldsListView::focusInEvent(QFocusEvent *event)
{
	if (selectedIndexes().length() > 0)
	{
		BOOST_FOREACH(AssignButton *button, _assignButtons)
			button->setAssignDirection(true);
	}

	QListView::focusInEvent(event);
}

void AvailableFieldsListView::selectionChanged(const QItemSelection &selected, const QItemSelection &deselected)
{
	if (selectedIndexes().length() > 0)
	{
		BOOST_FOREACH(AssignButton *button, _assignButtons)
			button->setAssignDirection(true);
	}

	QListView::selectionChanged(selected, deselected);
}

