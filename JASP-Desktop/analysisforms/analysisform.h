#ifndef ANALYSISFORM_H
#define ANALYSISFORM_H

#include <QWidget>
#include <QVBoxLayout>
#include <QPushButton>
#include <QMap>

#include "dataset.h"
#include "options.h"
#include "options/optionfields.h"

#include "availablefields.h"
#include "widgets/availablefieldslistview.h"
#include "widgets/assignbutton.h"
#include "widgets/boundlistview.h"

#include "widgets/listmodelvariablesavailable.h"

class AnalysisForm : public QWidget
{
	Q_OBJECT

public:
	explicit AnalysisForm(QString name, QWidget *parent = 0);
	virtual void set(Options *options, DataSet *dataSet);

protected:

	DataSet *_dataSet;
	Options *_options;

	ListModelVariablesAvailable _availableFields;

	OptionFields *_mainFields;
	
	
};

#endif // ANALYSISFORM_H
