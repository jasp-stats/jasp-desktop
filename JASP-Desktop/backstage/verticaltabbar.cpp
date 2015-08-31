
#include "verticaltabbar.h"

#include <QPushButton>
#include <QStyleOption>
#include <QPainter>

VerticalTabBar::VerticalTabBar(QWidget *parent) : QWidget(parent)
{
	_selectedIndex = 0;
	_buttonGroup = new QButtonGroup(this);

	_layout = new QVBoxLayout(this);
	_layout->addSpacerItem(new QSpacerItem(0, 0, QSizePolicy::Minimum, QSizePolicy::Expanding));
	setLayout(_layout);

	connect(_buttonGroup, SIGNAL(buttonClicked(int)), this, SLOT(buttonClicked(int)));
}

void VerticalTabBar::addTab(const QString &label, const QIcon &icon)
{
	QPushButton *button = new QPushButton(icon, label, this);
	button->setCheckable(true);

	_buttonGroup->addButton(button, _buttonGroup->buttons().count());
	_layout->insertWidget(_layout->count() - 1, button);

	if (_buttonGroup->buttons().count() == 1)
		button->setChecked(true);
}

void VerticalTabBar::showTab(int index)
{
	QAbstractButton *button = _buttonGroup->button(index);
	button->show();

	if (index == _selectedIndex)
	{
		button->setChecked(true);
		emit currentChanged(index);
	}
}

bool VerticalTabBar::isTabVisible(int index)
{
	return _buttonGroup->button(index)->isVisible();
}

void VerticalTabBar::hideTab(int index)
{	
	if (index == currentIndex())
	{
		for (int i = 0; i < count(); i++)
		{
			if (i == index)
				continue;

			if (isTabVisible(i))
			{
				_buttonGroup->button(i)->setChecked(true);
				_buttonGroup->button(index)->hide();
				emit currentChanged(i);
				return;
			}
		}
	}

	_buttonGroup->button(index)->hide();
}

void VerticalTabBar::setTabEnabled(int index, bool enable)
{
	_buttonGroup->button(index)->setEnabled(enable);
}

void VerticalTabBar::setCurrentIndex(int index)
{
	_selectedIndex = index;
	return _buttonGroup->button(index)->setChecked(true);
}

int VerticalTabBar::currentIndex() const
{
	return _buttonGroup->checkedId();
}

int VerticalTabBar::count() const
{
	return _buttonGroup->buttons().length();
}

void VerticalTabBar::paintEvent(QPaintEvent *)
{
	// subclassed QWidgets don't paint stuff from their stylesheet, so we have to do this

	QStyleOption option;
	option.init(this);
	QPainter painter(this);
	style()->drawPrimitive(QStyle::PE_Widget, &option, &painter, this);
}

void VerticalTabBar::buttonClicked(int id)
{
	if (_selectedIndex == id)
		return;

	bool cancel = false;
	emit currentChanging(id, cancel);

	if (cancel)
	{
		_buttonGroup->button(_selectedIndex)->setChecked(true);
	}
	else
	{
		_selectedIndex = id;
		emit currentChanged(id);
	}
}

