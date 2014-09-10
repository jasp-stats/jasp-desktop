#ifndef BACKSTAGEFORM_H
#define BACKSTAGEFORM_H

#include <QWidget>
#include <QSettings>

#include "common.h"

namespace Ui {
class BackStageForm;
}

class BackStageForm : public QWidget
{
    Q_OBJECT
    
public:
    explicit BackStageForm(QWidget *parent = 0);
    ~BackStageForm();

	bool eventFilter(QObject *object, QEvent *event) OVERRIDE;

public slots:
	void setFileLoaded(bool loaded);

signals:
	void dataSetSelected(QString filename);
	void closeDataSetSelected();
	void exportSelected(QString filename);
    
private:
    Ui::BackStageForm *ui;
	QSettings _settings;
	QStringList _recents;

	const int _maxRecents = 5;

private slots:
    void fileItemSelected();
	void closeItemSelected();
	void exportItemSelected();
	void exampleSelectedHandler(QString path);
	void recentSelectedHandler(QString path);

	void loadRecents();
	void loadExamples();

	void addToRecentList(QString path);

};

#endif // BACKSTAGEFORM_H
