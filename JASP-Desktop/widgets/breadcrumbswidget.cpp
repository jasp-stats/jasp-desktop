
#include "breadcrumbswidget.h"

#include <QPushButton>
#include <QLabel>

BreadCrumbsWidget::BreadCrumbsWidget(QWidget *parent) : QWidget(parent)
{
	_layout = new QHBoxLayout(this);
	_layout->setSpacing(2);
	_layout->setMargin(0);

	QPushButton *button;
	QLabel *label;

	button = new QPushButton("Documents", this);
	_layout->addWidget(button);

	//label = new QLabel("/", this);
	//_layout->addWidget(label);

	button = new QPushButton("Research", this);
	_layout->addWidget(button);

	//label = new QLabel("/", this);
	//_layout->addWidget(label);

	button = new QPushButton("Bruce McClelland", this);
	_layout->addWidget(button);

	_layout->addSpacerItem(new QSpacerItem(0, 0, QSizePolicy::Expanding, QSizePolicy::Minimum));
}

