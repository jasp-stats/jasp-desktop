
#include "datasetselectwidget.h"
#include "ui_datasetselectwidget.h"


DataSetSelectWidget::DataSetSelectWidget(QWidget *parent) :
	QPushButton(parent),
	ui(new Ui::DataSetSelectWidget)
{
	ui->setupUi(this);

	connect(this, SIGNAL(clicked()), this, SLOT(clickedHandler()));

	ui->icon->installEventFilter(this);
	ui->name->installEventFilter(this);
	ui->description->installEventFilter(this);

#ifdef __WIN32__
	QFont f = ui->name->font();
	QFont nf(f.family(), 11, f.weight(), f.italic());
	ui->name->setFont(nf);

	f = ui->description->font();
	QFont df(f.family(), 8, f.weight(), f.italic());
	ui->description->setFont(df);
#else
	QFont f = ui->description->font();
	QFont df(f.family(), 10, f.weight(), f.italic());
	ui->description->setFont(df);
#endif
}

DataSetSelectWidget::~DataSetSelectWidget()
{
	delete ui;
}

void DataSetSelectWidget::setDataSetName(QString name)
{
	ui->name->setText(name);
}

void DataSetSelectWidget::setDataSetPath(QString path)
{
	_path = path;
}

void DataSetSelectWidget::setDataSetDescription(QString description)
{
	ui->description->setText(description);
}

bool DataSetSelectWidget::eventFilter(QObject *object, QEvent *event)
{
	if (children().contains(object) && event->type() == QEvent::MouseButtonPress)
	{
		clickedHandler();
		return true;
	}
	else
	{
		return QPushButton::eventFilter(object, event);
	}
}

void DataSetSelectWidget::clickedHandler()
{
	emit dataSetSelected(_path);
}

