#ifndef REQUIREDPACKAGE_H
#define REQUIREDPACKAGE_H

#include "descriptionchildbase.h"
#include "../upgrader/version.h"
#include "jsonredirect.h"

namespace Modules
{

struct PackageError  : public std::runtime_error
{
	PackageError(QString pkg, QString problem);
	const char* what() const noexcept override;
};

class RequiredPackage : public DescriptionChildBase
{
	Q_OBJECT

	Q_PROPERTY(QString name		READ name		WRITE setName		NOTIFY nameChanged		)
	Q_PROPERTY(QString version	READ version	WRITE setVersion	NOTIFY versionChanged	)
	Q_PROPERTY(QString github	READ github		WRITE setGithub		NOTIFY githubChanged	)
	Q_PROPERTY(QString gitref	READ gitref		WRITE setGitref		NOTIFY gitrefChanged	)

public:
	RequiredPackage() : DescriptionChildBase() {}

	QString name()			const { return _name;						}
	QString version()		const { return _version;					}
	QString github()		const { return _github;						}
	QString gitref()		const { return _gitref;						}

	Json::Value	asJson() const;


public slots:
	void setName(		QString name);
	void setVersion(	QString version);
	void setGithub(		QString github);
	void setGitref(		QString gitref);

signals:
	void nameChanged(	QString name);
	void versionChanged();
	void githubChanged(	QString github);
	void gitrefChanged(	QString gitref);

private:
	QString _name,
			_github,
			_gitref,
			_version;
};

}

#endif // REQUIREDPACKAGE_H
