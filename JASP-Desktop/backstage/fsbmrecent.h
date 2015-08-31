#ifndef FSBROWSERMODELRECENT_H
#define FSBROWSERMODELRECENT_H

#include "fsbmodel.h"

#include <QSettings>

#include "common.h"

class FSBMRecent : public FSBModel
{
public:
	FSBMRecent(QObject *parent = NULL);

	void refresh() OVERRIDE;

	void addRecent(const QString &path);

protected:
	bool eventFilter(QObject *object, QEvent *event) OVERRIDE;

private:
	QStringList load();
	void populate(const QStringList &paths);

	QSettings _settings;

};

#endif // FSBROWSERMODELRECENT_H
