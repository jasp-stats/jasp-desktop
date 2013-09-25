#ifndef ONESAMPLETTESTFORM_H
#define ONESAMPLETTESTFORM_H

#include <QWidget>
#include "analysisform.h"

namespace Ui {
class TTestOneSampleForm;
}

class TTestOneSampleForm : public AnalysisForm
{
	Q_OBJECT
	
public:
	explicit TTestOneSampleForm(QWidget *parent = 0);
	~TTestOneSampleForm();
	
private:
	Ui::TTestOneSampleForm *ui;
};

#endif // ONESAMPLETTESTFORM_H
