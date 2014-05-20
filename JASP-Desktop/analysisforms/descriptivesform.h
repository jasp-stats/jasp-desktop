#ifndef DESCRIPTIVESFORM_H
#define DESCRIPTIVESFORM_H

#include <QWidget>

#include "analysisform.h"

using namespace std;

namespace Ui {
class DescriptivesForm;
}

class DescriptivesForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit DescriptivesForm(QWidget *parent = 0);
	~DescriptivesForm();

private:
	Ui::DescriptivesForm *ui;

};

#endif // DESCRIPTIVESFORM_H
