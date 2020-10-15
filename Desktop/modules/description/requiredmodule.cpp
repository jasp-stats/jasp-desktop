#include "requiredmodule.h"
#include "gui/preferencesmodel.h"

namespace Modules
{

ModuleError::ModuleError(QString pkg, QString problem)	: std::runtime_error("Pkg " + fq(pkg) + " has a problem: " + fq(problem)) {}

const char * ModuleError::what() const noexcept { return std::runtime_error::what(); }

void RequiredModule::setName(QString name)
{
	if (_name == name)
		return;

	_name = name;
	emit nameChanged(_name);
	emit somethingChanged(this);
}

}
