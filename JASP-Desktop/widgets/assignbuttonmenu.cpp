#include "assignbuttonmenu.h"

#include "droptarget.h"

#include <QMenu>
#include <QDebug>

AssignButtonMenu::AssignButtonMenu(QWidget *parent)
	: Button(parent)
{
	_source = NULL;
	_target = NULL;

	connect(this, SIGNAL(clicked()), this, SLOT(buttonClicked()));

	setIcon(QIcon(":/images/arrow-right.png"));

	setEnabled(false);
}

void AssignButtonMenu::setSourceAndTarget(DropTarget *source, DropTarget *target)
{
	_source = source;
	_target = target;

	_source->selectionUpdated.connect(boost::bind(&AssignButtonMenu::sourceChanged, this));
	_source->focused.connect(boost::bind(&AssignButtonMenu::sourceChanged, this));

	_target->selectionUpdated.connect(boost::bind(&AssignButtonMenu::targetChanged, this));
	_target->focused.connect(boost::bind(&AssignButtonMenu::targetChanged, this));
}

void AssignButtonMenu::buttonClicked()
{
	QMenu *m = menu();

	if (m != NULL)
		m->show();

	if (_source == NULL || _target == NULL)
	{
		qDebug() << "AssignButtonMenu::buttonClicked() : source or target not set";
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

	if (_source->hasSelection())
	{
		QMimeData *mimeData = _source->view()->model()->mimeData(_source->view()->selectionModel()->selectedIndexes());
		bool canAssign = _target->view()->model()->canDropMimeData(mimeData, Qt::MoveAction, -1, 0, QModelIndex());
		this->setEnabled(canAssign);
	}
	else
	{
		setEnabled(false);
	}
}

void AssignButtonMenu::targetChanged()
{
	setEnabled(false);
}



