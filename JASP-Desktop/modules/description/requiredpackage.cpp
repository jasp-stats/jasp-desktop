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

void RequiredPackage::setVersion(QString version)
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

	if (_github == github)
		return;

	_github = github;
	emit githubChanged(_github);
	emit somethingChanged(this);
}

void RequiredPackage::setGitref(QString gitref)
{
	if (_gitref == gitref)
		return;

	_gitref = gitref;
	emit gitrefChanged(_gitref);
	emit somethingChanged(this);
}

Json::Value RequiredPackage::asJson() const
{
	Json::Value json(Json::objectValue);

	json["package"] = fq(_name);

	if(_version != "")
		json["version"] = fq(_version);

	if(_github != "")
	{
		json["github"] = fq(_github + "/" + _name);

		if(_gitref != "")
			json["gitref"] = fq(_gitref);
	}

	return json;
}

}
