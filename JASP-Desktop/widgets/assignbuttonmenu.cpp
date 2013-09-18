#include "assignbuttonmenu.h"

#include <QDebug>

AssignButtonMenu::AssignButtonMenu(QWidget *parent)
	: QPushButton(parent)
{
	_source = NULL;
	_target = NULL;

	connect(this, SIGNAL(clicked()), this, SLOT(buttonClicked()));

	setIcon(QIcon(":/images/arrow-right.png"));

	setEnabled(false);
}

void AssignButtonMenu::setSourceAndTarget(ListView *source, ListView *target)
{
	_source = source;
	_target = target;

	connect(source, SIGNAL(focused()), this, SLOT(sourceChanged()));
	connect(source, SIGNAL(selectionUpdated()), this, SLOT(sourceChanged()));
	connect(target, SIGNAL(focused()), this, SLOT(targetChanged()));
	connect(target, SIGNAL(selectionUpdated()), this, SLOT(targetChanged()));
}

void AssignButtonMenu::buttonClicked()
{
	QMenu *m = menu();

	if (m != NULL)
		m->show();

	if (_source == NULL || _target == NULL)
	{
		qDebug() << "AssignButtonMenu::buttonClicked() : source and target not set";
		return;
	}
}

void AssignButtonMenu::sourceChanged()
{
	if (_target == NULL)
	{
		qDebug() << "AssignButtonMenu::sourceChanged() : target not set";
		return;
	}

	if (_source->selectionModel()->selectedIndexes().size() == 0)
	{
		setEnabled(false);
	}
	else
	{
		QMimeData *mimeData = _source->model()->mimeData(_source->selectionModel()->selectedIndexes());
		bool canAssign = _target->model()->canDropMimeData(mimeData, Qt::MoveAction, -1, 0, QModelIndex());
		this->setEnabled(canAssign);
	}
}

void AssignButtonMenu::targetChanged()
{
	setEnabled(false);

	if (_source == NULL)
	{
		qDebug() << "AssignButtonMenu::targetChanged() : source not set";
		return;
	}
}



