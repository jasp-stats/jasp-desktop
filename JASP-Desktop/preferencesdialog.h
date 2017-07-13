#ifndef PREFERENCESDIALOG_H
#define PREFERENCESDIALOG_H

#include "widgets/tabbar.h"
#include <QDialog>
#include <QSettings>
#include <QFileDialog>

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

public slots:
	void savePreferences();
	void setDefaultEditorCheck(bool defaulteditor);
	void getSpreadsheetEditor(); 

};

#endif // PREFERENCESDIALOG_H
