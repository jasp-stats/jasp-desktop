#include "anovamodelwidget.h"
#include "ui_anovamodelwidget.h"

#include <boost/foreach.hpp>
#include <QDebug>

#include "analysisforms/analysisform.h"

using namespace std;

AnovaModelWidget::AnovaModelWidget(QWidget *parent) :
	QWidget(parent),
	ui(new Ui::AnovaModelWidget),
	_availableFields(this)
{
	ui->setupUi(this);

	_boundTo = NULL;
	_optionFixedFactors = NULL;
	_optionRandomFactors = NULL;

	ui->listFactorsAvailable->setModel(&_availableFields);
	//ui->interactions->setModel(&_interactionsModel);

	AnalysisForm::link(ui->listFactorsAvailable, ui->buttonAssignMainEffects, ui->mainEffects);
	AnalysisForm::link(ui->listFactorsAvailable, ui->buttonAssignInteractions, ui->interactions);

}

AnovaModelWidget::~AnovaModelWidget()
{
	delete ui;
}

void AnovaModelWidget::bindTo(Option *option)
{
	_boundTo = dynamic_cast<OptionString *>(option);
}

void AnovaModelWidget::bindTo(Option *option, int item)
{
	if (item == FIXED_FACTORS)
	{
		_optionFixedFactors = dynamic_cast<OptionFields *>(option);
		if (_optionFixedFactors != NULL)
			_optionFixedFactors->changed.connect(boost::bind(&AnovaModelWidget::optionChangedHandler, this, _1));
		else
			qDebug() << "Option* could not be cast to OptionFields* : bindTo(Option*, int)";
	}
	else if (item == RANDOM_FACTORS)
	{
		_optionRandomFactors = dynamic_cast<OptionFields *>(option);
		if (_optionRandomFactors != NULL)
			_optionRandomFactors->changed.connect(boost::bind(&AnovaModelWidget::optionChangedHandler, this, _1));
		else
			qDebug() << "Option* could not be cast to OptionFields* : bindTo(Option*, int)";
	}
	else if (item == MAIN_EFFECTS)
	{
		ui->mainEffects->bindTo(option);
	}
}

void AnovaModelWidget::setDataSet(DataSet *dataSet)
{
	_availableFields.setDataSet(dataSet);
	_availableFields.filter(vector<string>());

	ui->mainEffects->setDataSet(dataSet);
	ui->interactions->setDataSet(dataSet);
}

void AnovaModelWidget::optionChangedHandler(Option *option)
{
	if (option == _optionFixedFactors || option == _optionRandomFactors)
	{
		vector<string> factors;

		BOOST_FOREACH(string factor, _optionFixedFactors->value())
			factors.push_back(factor);

		BOOST_FOREACH(string factor, _optionRandomFactors->value())
			factors.push_back(factor);

		_availableFields.filter(factors);
	}

}
