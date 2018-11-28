#ifndef PREFERENCESDIALOG_H
#define PREFERENCESDIALOG_H

#include "widgets/tabbar.h"
#include "column.h"
#include "utilities/qutils.h"
#include <QDialog>
#include <QFileDialog>
#include <QSizePolicy>
#include <QButtonGroup>
#include <QAbstractButton>

namespace Ui {
class PreferencesDialog;
}

class PreferencesDialog : public QDialog
{
	Q_OBJECT

public:
	explicit PreferencesDialog(QWidget *parent = 0);
	~PreferencesDialog();

	void setDefaultPPI(int ppi);

private:
	Ui::PreferencesDialog *ui;
	TabBar *_tabBar;
	QButtonGroup* _imageBackgroundGroup;
	static int _currentTab;
	std::vector<std::string> getStdVectorFromEmptyValueList();
	QString getTokenStringFromEmptyValueList();
	bool addStringToEmptyValueList(const QString &in);
	void checkEmptyValueList();
	void fillMissingValueList(const std::vector<std::string> &emptyValues);
	void setSliderUIScale(float scale);
	float sliderUIScale();
	
public slots:
	void savePreferences();
	void setDefaultEditorCheck(bool defaulteditor);
	void getSpreadsheetEditor();
<<<<<<< HEAD:JASP-Desktop/preferencesdialog.h
	void sliderUIScaleChanged(int value);
=======
	void getQMLFile();
	void getRFile();
>>>>>>> qmlFormsB:JASP-Desktop/gui/preferencesdialog.h

protected:
	void virtual showEvent(QShowEvent * event);

private slots:
	void on_fixDecimals_clicked();
	void on_addPushButton_clicked();
	void on_deletePushButton_clicked();
	void on_resetPushButton_clicked();
	void currentTabChanged(int tabNr);

};

#endif // PREFERENCESDIALOG_H
