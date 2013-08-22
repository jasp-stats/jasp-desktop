#ifndef ANOVAMULTIVARIATEFORM_H
#define ANOVAMULTIVARIATEFORM_H

#include "analysisform.h"

namespace Ui {
class AnovaMultivariateForm;
}

class AnovaMultivariateForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit AnovaMultivariateForm(QWidget *parent = 0);
	~AnovaMultivariateForm();
	
private:
	Ui::AnovaMultivariateForm *ui;
};

#endif // ANOVAMULTIVARIATEFORM_H
