#ifndef ANOVAREPEATEDMEASURESFORM_H
#define ANOVAREPEATEDMEASURESFORM_H

#include <QWidget>

namespace Ui {
class AnovaRepeatedMeasuresForm;
}

class AnovaRepeatedMeasuresForm : public QWidget
{
	Q_OBJECT

public:
	explicit AnovaRepeatedMeasuresForm(QWidget *parent = 0);
	~AnovaRepeatedMeasuresForm();

private:
	Ui::AnovaRepeatedMeasuresForm *ui;
};

#endif // ANOVAREPEATEDMEASURESFORM_H
