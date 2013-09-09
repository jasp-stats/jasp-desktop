#include "analysisform.h"

#include "boost/foreach.hpp"

#include "bound.h"

#include <QLabel>
#include <QTimer>
#include <QResizeEvent>
#include "widgets/boundlistview.h"
#include "widgets/boundpairstable.h"

AnalysisForm::AnalysisForm(QWidget *parent) :
	QWidget(parent),
	_availableFields(parent)
{
	_mainFields = NULL;

	_buttonPanel = new QWidget(this);
	_buttonPanelLayout = new QVBoxLayout(_buttonPanel);
	_buttonPanelLayout->setSpacing(6);
	_buttonPanel->setLayout(_buttonPanelLayout);

	_okButton = new QPushButton(QString("OK"), _buttonPanel);
	_removeButton = new QPushButton(QString("Remove"), _buttonPanel);

	_buttonPanelLayout->addWidget(_okButton);
	_buttonPanelLayout->addWidget(_removeButton);

	_buttonPanel->resize(_buttonPanel->sizeHint());
	_buttonPanel->move(0, 0);

	QTimer::singleShot(0, this, SLOT(repositionButtonPanel()));
	connect(_okButton, SIGNAL(clicked()), this, SLOT(accept()));
}

void AnalysisForm::accept()
{
	emit accepted();
}

void AnalysisForm::repositionButtonPanel()
{
	int overallWidth = this->width();
	int panelWidth = _buttonPanel->width();

	_buttonPanel->move(overallWidth - panelWidth, 0);
	_buttonPanel->raise();
}

void AnalysisForm::set(Options *options, DataSet *dataSet)
{
	_dataSet = dataSet;

	_availableFields.setDataSet(dataSet);
	_options = options;

	BOOST_FOREACH(Option *option, *options)
	{
		OptionFields *fieldsOption = dynamic_cast<OptionFields*>(option);
		if (fieldsOption != NULL)
		{
			if (fieldsOption->name() == "main/fields")
				_mainFields = fieldsOption;
			_availableFields.provideFor(fieldsOption);
		}

		QString name = QString::fromUtf8(option->name().c_str(), option->name().length());
		name.replace('/', '_');

		QWidget *child = this->findChild<QWidget*>(name);

		Bound *boundChild = dynamic_cast<Bound*>(child);

		if (boundChild != NULL)
		{
			boundChild->bindTo(option);
			boundChild->setDataSet(dataSet);
		}
		else
			qDebug() << "child not found : " << name << " in AnalysisForm::setOptions()";
	}
}

void AnalysisForm::link(AvailableFieldsListView *source, AssignButton *assign, BoundListView *target)
{
	source->addAssignButton(assign);
	target->setAssignButton(assign);
	target->setAvailableFieldsListView(source);
}


void AnalysisForm::resizeEvent(QResizeEvent *event)
{
	repositionButtonPanel();
}
