
#include "tabbar.h"

TabBar::TabBar(QWidget *parent) :
	QWidget(parent)
{
	_optionsTab = NULL;
	_helpTab = NULL;

	_background = new QWidget(this);
	_background->setObjectName("background");

	_backgroundLayout = new QGridLayout(this);
	_backgroundLayout->setMargin(0);
	_backgroundLayout->addWidget(_background);

	_layout = new QHBoxLayout(_background);
	_layout->setMargin(0);
	_background->setLayout(_layout);

	setLayout(_backgroundLayout);

	_layout->addStretch(1);
}

void TabBar::addTab(QString tabName)
{
	QPushButton *button = new QPushButton(tabName, this);
	button->setCheckable(true);
	connect(button, SIGNAL(clicked()), this, SLOT(tabSelectedHandler()));

	if (_tabButtons.size() == 0)
		button->setObjectName("first");

	_layout->insertWidget(_tabButtons.size(), button);
	_tabButtons.append(button);
}

void TabBar::removeTab(int index)
{
	QPushButton *button = _tabButtons.at(index);
	_tabButtons.removeAt(index);
	delete button;
}

void TabBar::addOptionsTab()
{
	_optionsTab = new QPushButton("Options", this);
	_optionsTab->setCheckable(true);
	connect(_optionsTab, SIGNAL(clicked()), this, SLOT(tabSelectedHandler()));

	_layout->addWidget(_optionsTab);
}

void TabBar::addHelpTab()
{
	_helpTab = new QPushButton("Help", this);
	_helpTab->setObjectName("help");
	_helpTab->setCheckable(true);
	connect(_helpTab, SIGNAL(toggled(bool)), this, SLOT(helpToggledHandler(bool)));

	_layout->addWidget(_helpTab);
}

int TabBar::count() const
{
	return _tabButtons.length() + (_optionsTab != NULL ? 1 : 0);
}

void TabBar::setCurrentIndex(int index)
{
	int i = 0;

	foreach (QPushButton *button, _tabButtons)
	{
		button->setChecked(i == index);
		i++;
	}

	if (_optionsTab != NULL)
		_optionsTab->setChecked(i == index);

	emit currentChanged(index);
}

void TabBar::tabSelectedHandler()
{
	QObject *source = this->sender();
	int i = 0;

	foreach (QPushButton *button, _tabButtons)
	{
		if (source == button)
		{
			setCurrentIndex(i);
			return;
		}
		i++;
	}

	if (source == _optionsTab)
		setCurrentIndex(i);
}

void TabBar::helpToggledHandler(bool on)
{
	emit helpToggled(on);
}
