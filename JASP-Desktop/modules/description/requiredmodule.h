#ifndef REQUIREDMODULE_H
#define REQUIREDMODULE_H

#include "descriptionchildbase.h"
#include "../upgrader/version.h"
#include "jsonredirect.h"

namespace Modules
{

struct ModuleError  : public std::runtime_error
{
	ModuleError(QString pkg, QString problem);
	const char* what() const noexcept override;
};

class RequiredModule : public DescriptionChildBase
{
	Q_OBJECT

	Q_PROPERTY(QString name		READ name		WRITE setName		NOTIFY nameChanged		)

public:
	RequiredModule() : DescriptionChildBase() {}

	QString name()			const { return _name;						}

public slots:
	void setName(		QString name);

signals:
	void nameChanged(	QString name);

private:
	QString _name;
};

}

#endif // REQUIREDMODULE_H
