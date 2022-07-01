#include "aboutmodel.h"
#include "appinfo.h"
#include "qutils.h"
#include "mainwindow.h"

bool AboutModel::visible() const
{
	return _visible;
}

QString AboutModel::version()
{
	return MainWindow::versionString();
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
	return tq("JASP Team (" +  AppInfo::getBuildYear() + "). JASP (Version " + AppInfo::version.asString() +
		#ifdef JASP_DEBUG
			"-Debug-" + AppInfo::gitCommit //If it is a debug version that is actually being cited (highly unusual I suppose and downright dangerous, but ok.) In that scenario I'm pretty sure one would want to know the exact commit even though it might be ugly.
		#endif
			+ ") [Computer software].");
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

