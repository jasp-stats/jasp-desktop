#ifndef BACKSTAGECURRENTFILE_H
#define BACKSTAGECURRENTFILE_H

#include "backstagepage.h"
#include "currentfilelistmodel.h"

namespace Ui {
class BackstageForm;
}

class BackstageCurrentFile : public BackstagePage
{
	Q_OBJECT
	
public:
	explicit BackstageCurrentFile(QWidget *parent = 0);
	~BackstageCurrentFile();
	
	void setCurrentFilePath(const QString &path);
	void setCurrentDataFilePath(const QString &path);
    void setCurrentFileInfo(const QString &path, const Utils::FileType &type, const bool & readonly);
	void setCurrentFileType(const Utils::FileType &type);
	void setCurrentFileReadOnly(const bool & readonly);
	
	Utils::FileType getCurrentFileType();	
	bool isCurrentFileReadOnly();
	bool isOnlineFile(const QString &path);
		
	//Special
	CurrentFileListModel * getCurrentFileListModel();

public slots:
	QString getCurrentFilePath();
	QString getCurrentDataFilePath();
	QString getCurrentDataFileName();
	QString getCurrentDataFolder();
	QString getHeaderText();		
	void syncFile(FileEvent *event);
	
private:
	CurrentFileListModel *_currentFileListModel;
	
	QString _currentFilePath;
	QString _currentDataFilePath;
	Utils::FileType _currentFileType;
	bool _currentFileReadOnly;
		
	Ui::BackstageForm *ui;
};

#endif // BACKSTAGECURRENTFILE_H
