#ifndef ONEWAYANOVAFORM_H
#define ONEWAYANOVAFORM_H

#include "options/options.h"
#include "dataset.h"

#include "analysisform.h"

#include "availablefields.h"
#include "widgets/tablemodelcontrasts.h"

namespace Ui {
class AnovaOneWayForm;
}

class AnovaOneWayForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit AnovaOneWayForm(QWidget *parent = 0);
	~AnovaOneWayForm();

private slots:
	void groupingVariableChanged();
	void contrastsClicked(QModelIndex index);

private:
	Ui::AnovaOneWayForm *ui;

	TableModelVariablesAssigned _variablesModel;
	TableModelVariablesAssigned _groupingVariableModel;
	TableModelContrasts _contrastsModel;

};

#endif // ONEWAYANOVAFORM_H
