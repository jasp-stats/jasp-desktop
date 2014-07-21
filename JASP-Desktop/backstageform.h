#ifndef BACKSTAGEFORM_H
#define BACKSTAGEFORM_H

#include <QWidget>
#include <QSettings>

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
	void exportSelected(QString filename);
    
private:
    Ui::BackStageForm *ui;
	QSettings _settings;

private slots:
    void fileItemSelected();
	void closeItemSelected();
	void exportItemSelected();

};

#endif // BACKSTAGEFORM_H
