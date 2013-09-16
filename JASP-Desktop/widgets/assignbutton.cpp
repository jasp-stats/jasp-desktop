#include "assignbutton.h"

#include <QMimeData>
#include "draganddrop.h"

AssignButton::AssignButton(QWidget *parent) :
	QPushButton(parent)
{
	_source = NULL;
	_target = NULL;

	connect(this, SIGNAL(clicked()), this, SLOT(buttonClicked()));

	setAssignDirection(true);
}

void AssignButton::setAssignDirection(bool assign)
{
	_assignDirection = assign;

	if (assign)
		setIcon(QIcon(":/images/arrow-right.png"));
	else
		setIcon(QIcon(":/images/arrow-left.png"));
}

bool AssignButton::isAssign()
{
	return _assignDirection;
}

void AssignButton::setSourceAndTarget(ListView *source, ListView *target)
{
	_source = source;
	_target = target;

	connect(source, SIGNAL(focused()), this, SLOT(sourceChanged()));
	connect(source, SIGNAL(selectionUpdated()), this, SLOT(sourceChanged()));
	connect(target, SIGNAL(focused()), this, SLOT(targetChanged()));
	connect(target, SIGNAL(selectionUpdated()), this, SLOT(targetChanged()));
}

void AssignButton::setSourceAndTarget(ListView *source, BoundPairsTable *target)
{
	_source = source;
	_target = target;

	connect(source, SIGNAL(focused()), this, SLOT(sourceChanged()));
	connect(source, SIGNAL(selectionUpdated()), this, SLOT(sourceChanged()));
	connect(target, SIGNAL(focused()), this, SLOT(targetChanged()));
	connect(target, SIGNAL(selectionUpdated()), this, SLOT(targetChanged()));
}

void AssignButton::buttonClicked()
{
	if (_source == NULL || _target == NULL)
	{
		qDebug() << "AssignButton::buttonClicked() : source and target not set";
		return;
	}

	if (_assignDirection)
		DragAndDrop::perform(_source, _target);
	else
		DragAndDrop::perform(_target, _source);
}

void AssignButton::sourceChanged()
{	
	setAssignDirection(true);

	if (_target == NULL)
	{
		qDebug() << "AssignButton::sourceChanged() : target not set";
		return;
	}

	QMimeData *mimeData = _source->model()->mimeData(_source->selectionModel()->selectedIndexes());
	bool canAssign = _target->model()->canDropMimeData(mimeData, Qt::MoveAction, -1, 0, QModelIndex());
	this->setEnabled(canAssign);
}

void AssignButton::targetChanged()
{	
	setAssignDirection(false);

	if (_source == NULL)
	{
		qDebug() << "AssignButton::targetChanged() : source not set";
		return;
	}

	QMimeData *mimeData = _target->model()->mimeData(_target->selectionModel()->selectedIndexes());
	bool canAssign = _source->model()->canDropMimeData(mimeData, Qt::MoveAction, -1, 0, QModelIndex());
	this->setEnabled(canAssign);
}



