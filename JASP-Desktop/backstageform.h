#ifndef BACKSTAGEFORM_H
#define BACKSTAGEFORM_H

#include <QWidget>
#include <QSettings>

#include "common.h"
#include "activitylog.h"

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

	void setLog(ActivityLog *log);

public slots:
	void setFileLoaded(bool loaded, QString filename);
	void openFile();
	bool saveAs();
	bool save();

signals:
	void dataSetSelected(QString filename);
	void closeDataSetSelected();
	void exportSelected(QString filename);
	void saveSelected(QString filename);
    
private:
    Ui::BackStageForm *ui;
	QSettings _settings;
	QStringList _recents;
	QString _filename;

	ActivityLog *_log;
	const int _maxRecents = 5;
	bool _loaded = false;

private slots:
	void closeItemSelected();
	void exportItemSelected();
	void exampleSelectedHandler(QString path);
	void recentSelectedHandler(QString path);

	void loadRecents();
	void loadExamples();

	void addToRecentList(QString path);

};

#endif // BACKSTAGEFORM_H
