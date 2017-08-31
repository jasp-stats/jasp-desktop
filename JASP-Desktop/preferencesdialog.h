#ifndef PREFERENCESDIALOG_H
#define PREFERENCESDIALOG_H

#include "widgets/tabbar.h"
#include "column.h"
#include "qutils.h"
#include <QDialog>
#include <QSettings>
#include <QFileDialog>

#define EMPTY_VALUE "<empty>"
#define SPACE_VALUE "<spaces>"

namespace Ui {
class PreferencesDialog;
}

class PreferencesDialog : public QDialog
{
	Q_OBJECT

public:
	explicit PreferencesDialog(QWidget *parent = 0);
	~PreferencesDialog();


private:
	Ui::PreferencesDialog *ui;
	QSettings _settings;
	TabBar *_tabBar;
	std::vector<std::string> getStdVectorFromEmptyValueList();
	QString getTokenStringFromEmptyValueList();
	void setEmptValueListFromStdVector(const std::vector<std::string> &input);
	void setEmptValueListFromTokenString(const QString &in);
	bool addStringToEmptyValueList(const QString &in);
	
public slots:
	void savePreferences();
	void setDefaultEditorCheck(bool defaulteditor);
	void getSpreadsheetEditor(); 

private slots:
	void on_addPushButton_clicked();
	void on_deletePushButton_clicked();
	void on_resetPushButton_clicked();
	void on_buttonBox_accepted();

};

#endif // PREFERENCESDIALOG_H
