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

public slots:
	void setFileLoaded(bool loaded);

signals:
	void dataSetSelected(QString filename);
	void closeDataSetSelected();
    
private:
    Ui::BackStageForm *ui;
	QSettings _settings;

private slots:
    void fileItemSelected();
	void exitItemSelected();
	void closeItemSelected();

};

#endif // BACKSTAGEFORM_H
