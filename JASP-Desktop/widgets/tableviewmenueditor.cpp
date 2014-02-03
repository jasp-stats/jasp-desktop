#include "tableviewmenueditor.h"

#include <QPaintEvent>
#include <QPainter>

TableViewMenuEditor::TableViewMenuEditor(QWidget *parent) :
	QWidget(parent)
{
	_menu = new QMenu(this);
}

QString TableViewMenuEditor::selected()
{
	return _selected;
}

void TableViewMenuEditor::setMenuContent(QStringList menuItems, QString selected)
{
	_menu->clear();

	QAction *active;

	foreach (QString item, menuItems)
	{
		QAction *added = _menu->addAction(item);
		if (selected == item)
			active = added;
	}

	_menu->setActiveAction(active);
	connect(_menu, SIGNAL(triggered(QAction*)), this, SLOT(itemSelected(QAction*)));
}

void TableViewMenuEditor::showEvent(QShowEvent *)
{
	QPoint pos(rect().left() + 20, rect().top() - 5);
	_menu->popup(mapToGlobal(pos), _menu->activeAction());
}

void TableViewMenuEditor::paintEvent(QPaintEvent *event)
{
	QPainter painter(this);

	QPainterPath path;
	path.moveTo(5,  20 / 5);
	path.lineTo(16, 20 / 2);
	path.lineTo(5,  20 * 4 / 5);

	QBrush black(QColor(0x80, 0x80, 0x80));
	QBrush white(Qt::white);

	painter.fillRect(event->rect(), white);
	painter.fillPath(path, black);
}

void TableViewMenuEditor::itemSelected(QAction *action)
{
	_selected = action->text();
	emit editingFinished();
}
