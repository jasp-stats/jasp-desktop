#ifndef FSBROWSERMODELRECENTFOLDERS_H
#define FSBROWSERMODELRECENTFOLDERS_H

#include "fsbmodel.h"
#include "common.h"

#include <QSettings>

class FSBMRecentFolders : public FSBModel
{
public:
	explicit FSBMRecentFolders(QObject *parent = NULL);

	void refresh() OVERRIDE;

	QString mostRecent() const;

public slots:
	void addRecent(QString path);

private:

	QStringList readRecents();
	void setRecents(const QStringList &recents);
	void setAndSaveRecents(const QStringList &recents);
	void saveRecents();

	QStringList _recents;
	QSettings _settings;
};

#endif // FSBROWSERMODELRECENTFOLDERS_H
