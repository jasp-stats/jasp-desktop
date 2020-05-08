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
	Q_PROPERTY(Version version	READ version	WRITE setVersion	NOTIFY versionChanged	)
	Q_PROPERTY(QString github	READ github		WRITE setGithub		NOTIFY githubChanged	)

public:
	RequiredPackage() : DescriptionChildBase() {}

	QString name()		const { return _name;		}
	Version version()	const { return _version;	}
	QString github()	const { return _github;		}

	Json::Value	asJson() const;

public slots:
	void setName(	QString name);
	void setVersion(Version version);
	void setGithub(	QString github);

signals:
	void nameChanged(	QString name);
	void versionChanged(Version version);
	void githubChanged(	QString github);

private:
	QString _name,
			_github;
	Version _version;
};

}

#endif // REQUIREDPACKAGE_H
