#ifndef ANALYSISFORM_H
#define ANALYSISFORM_H

#include <QWidget>
#include <QVBoxLayout>
#include <QPushButton>
#include <QMap>

#include "dataset.h"
#include "options/options.h"
#include "options/optionvariables.h"

#include "availablefields.h"
#include "widgets/availablefieldslistview.h"
#include "widgets/assignbutton.h"
#include "widgets/boundlistview.h"

#include "widgets/tablemodelvariablesavailable.h"

#include "variableinfo.h"

class AnalysisForm : public QWidget, protected VariableInfoProvider
{
	Q_OBJECT

public:
	explicit AnalysisForm(QString name, QWidget *parent = 0);
	void bindTo(Options *options, DataSet *dataSet);
	void unbind();

protected:

	virtual QVariant requestInfo(const Term &term, VariableInfo::InfoType info) const OVERRIDE;

	DataSet *_dataSet;
	Options *_options;

	TableModelVariablesAvailable _availableVariablesModel;

	OptionVariables *_mainVariables;
	
	
};

#endif // ANALYSISFORM_H
