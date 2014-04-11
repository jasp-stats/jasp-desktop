#ifndef BACKSTAGEFORM_H
#define BACKSTAGEFORM_H

#include <QWidget>
#include <QSettings>

#include "asyncloader.h"
#include "dataset.h"

namespace Ui {
class BackStageForm;
}

class BackStageForm : public QWidget
{
    Q_OBJECT
    
public:
    explicit BackStageForm(QWidget *parent = 0);
    ~BackStageForm();

signals:
	void dataSetSelected(QString filename);
    
private:
    Ui::BackStageForm *ui;
	QSettings _settings;

private slots:
    void fileItemSelected();
	void exitItemSelected();

};

#endif // BACKSTAGEFORM_H
