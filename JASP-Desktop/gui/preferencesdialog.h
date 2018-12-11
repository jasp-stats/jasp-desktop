#ifndef PREFERENCESDIALOG_H
#define PREFERENCESDIALOG_H

#include "column.h"
#include "utilities/qutils.h"
#include <QSizePolicy>
#include <QButtonGroup>
#include <QAbstractButton>


class PreferencesDialog : public QObject
{
	Q_OBJECT

public:
	explicit PreferencesDialog(QObject *parent = 0);
	~PreferencesDialog();

	void setDefaultPPI(int ppi);

private:
	//TabBar *_tabBar;
	//QButtonGroup* _imageBackgroundGroup;
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
	void sliderUIScaleChanged(int value);
	void getQMLFile();
	void getRFile();

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
