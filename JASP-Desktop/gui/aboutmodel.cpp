#include "aboutmodel.h"
#include "appinfo.h"
#include "utilities/qutils.h"

bool AboutModel::visible() const
{
	return _visible;
}

QString AboutModel::version()
{
	return "Version " + tq(AppInfo::version.asString(true));
}

QString AboutModel::buildDate()
{
	return tq(AppInfo::builddate);
}

QString AboutModel::copyrightMessage()
{
	return "Copyright 2013-" + tq(AppInfo::getBuildYear()) + " University of Amsterdam";
}

QString AboutModel::citation()
{
	return "JASP Team (" +  tq(AppInfo::getBuildYear()) + "). JASP (Version " + tq(AppInfo::version.asString(true)) +") [Computer software].";
}

QString AboutModel::commit()
{
	return tq(AppInfo::gitCommit);
}

QString AboutModel::branch()
{
	return tq(AppInfo::gitBranch);
}

void AboutModel::setVisible(bool visible)
{
	if (_visible == visible)
		return;

	_visible = visible;
	emit visibleChanged(_visible);
}

