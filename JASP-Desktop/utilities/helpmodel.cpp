#include "helpmodel.h"
#include "appdirs.h"
#include <QFile>

void HelpModel::setVisible(bool visible)
{
	if (_visible == visible)
		return;

	_visible = visible;
	emit visibleChanged(_visible);
}


void HelpModel::setPageName(QString pageName)
{
	pageName = pageName.toLower(); //Otherwise we get into to trouble on systems that discern between cases in the filesystem. Also means we should make sure all documentation html are named lowercase!

	if (_pageName == pageName)
		return;

	_pageName = pageName;
	emit pageNameChanged(_pageName);
}

QString	HelpModel::indexURL()
{
	return "file:" + AppDirs::help() + "/index.html";
}

void HelpModel::generateJavascript()
{
	QFile fileMD(AppDirs::help() + "/" + _pageName + ".md"), fileHTML(AppDirs::help() + "/" + _pageName + ".html");

	QString content, renderFunc = "window.render";


	if (fileHTML.exists())
	{
		fileHTML.open(QFile::ReadOnly);
		content = QString::fromUtf8(fileHTML.readAll());
		fileHTML.close();

		renderFunc = "window.renderHtml";

	}
	else if (fileMD.exists())
	{
		fileMD.open(QFile::ReadOnly);
		content = QString::fromUtf8(fileMD.readAll());
		fileMD.close();
	}
	else
		content = "Coming Soon!\n========\n\nThere is currently no help available for this analysis"
#ifdef JASP_DEBUG
			 " ("+_pageName+")"
#endif
			".\n\nAdditional documentation will be available in future releases of JASP.";

	content.replace("\"", "\\\"");
	content.replace("\r\n", "\\n");
	content.replace("\r", "\\n");
	content.replace("\n", "\\n");

	setHelpJS(renderFunc + "(\"" + content + "\")");
}

void HelpModel::setHelpJS(QString helpJS)
{
	if (_helpJS == helpJS)
		return;

	_helpJS = helpJS;
	emit helpJSChanged(_helpJS);
}

void HelpModel::showOrTogglePage(QString pageName)
{
	if(pageName == _pageName && _visible)
		setVisible(false);
	else
	{
		setPageName(pageName);
		setVisible(true);
	}
}
