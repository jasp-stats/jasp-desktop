#include "requiredpackage.h"
#include "gui/preferencesmodel.h"

namespace Modules
{


PackageError::PackageError(QString pkg, QString problem)	: std::runtime_error("Pkg " + fq(pkg) + " has a problem: " + fq(problem)) {}

const char * PackageError::what() const noexcept { return std::runtime_error::what(); }


void RequiredPackage::setName(QString name)
{
	if (_name == name)
		return;

	_name = name;
	emit nameChanged(_name);
	emit somethingChanged(this);
}

void RequiredPackage::setVersion(Version version)
{
	if (_version == version)
		return;

	_version = version;
	emit versionChanged();
	emit somethingChanged(this);
}

void RequiredPackage::setGithub(QString github)
{
	if(github != "" && !PreferencesModel::prefs()->developerMode())
		throw PackageError(_name, "installing from GitHub is only allowed in Developer Mode!");

	throw std::runtime_error("RequiredPacakge::setGithub NOT IMPLEMENTED! well it is, but it isnt used anywhere so it wont work...");

	if (_github == github)
		return;

	_github = github;
	emit githubChanged(_github);
	emit somethingChanged(this);
}

Json::Value RequiredPackage::asJson() const
{
	Json::Value json(Json::objectValue);

	json["package"] = fq(_name);

	if(_version != Version())
		json["version"] = _version.toString();

	return json;
}

}
