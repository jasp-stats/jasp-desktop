#include "upgradeDefinitions.h"

namespace Modules
{

const std::string		prefixLog	= "\t\t";
const char			*	logId		= "!log!"; //To add general logging to UpgradeMsgs as opposed to for a specific option

const char * upgradeError::what() const noexcept
{
	//Just here to have an out-of-line virtual method so that clang and gcc don't complain so much
	return std::runtime_error::what();
}

const char * upgradeLoadError::what() const noexcept
{
	//Just here to have an out-of-line virtual method so that clang and gcc don't complain so much
	return std::runtime_error::what();
}

bool StepTaken::operator< (const StepTaken & other) const
{
	if(module <	other.module)	return true;
	if(module >	other.module)	return false;

	if(name < other.name)		return true;
	if(name > other.name)		return false;

	if(version < other.version) return true;

	return false;
}

}
