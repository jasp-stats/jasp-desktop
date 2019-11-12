#include "helpmodel.h"
#include "appdirs.h"
#include <QFile>
#include <QFileInfo>
#include <QDir>
#include "stringutils.h"

void HelpModel::setVisible(bool visible)
{
	if (_visible == visible)
		return;

	_visible = visible;
	emit visibleChanged(_visible);
}


void HelpModel::setPagePath(QString pagePath)
{
	pagePath = convertPagePathToLower(pagePath); //Otherwise we get into to trouble on systems that discern between cases in the filesystem. Also means we should make sure all documentation html are named lowercase!

	if (_pagePath == pagePath)
		return;

	_pagePath = pagePath;
	emit pagePathChanged(_pagePath);
}

QString	HelpModel::indexURL()
{
	return "file:" + AppDirs::help() + "/index.html";
}

void HelpModel::generateJavascript()
{
	QString content, renderFunc = "window.render";

	QFile fileMD, fileHTML;
	QFileInfo pathMd(_pagePath + ".md");

	bool relative = pathMd.isRelative();

	LanguageInfo li = LanguageModel::CurrentLanguageInfo;
	QString localnname = li.localName;
	QString _localname = localnname  == "en" ? "" : ("_" + localnname);

	if(relative) //This is probably a file in resources then
	{
		fileMD.setFileName(AppDirs::help() + "/" + _pagePath + _localname + ".md");
		fileHTML.setFileName(AppDirs::help() + "/" + _pagePath + _localname + ".html");
	}
	else
	{
		//We got an absolute path, this probably means it comes from a (dynamic) module.

		fileMD.setFileName(_pagePath + _localname + ".md");
		fileHTML.setFileName(_pagePath + _localname + ".html");
	}

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
	{
		content = tr("Coming Soon!\n========\n\nThere is currently no help available for this analysis");
#ifdef JASP_DEBUG
		content += 	 " (" + _pagePath + ")";
#endif
		content += tr(".\n\nAdditional documentation will be available in future releases of ");
		content += relative ? "JASP." : tr("the module.");
	}

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

QString HelpModel::convertPagePathToLower(const QString & pagePath)
{
	std::string pagePathStd		= pagePath.toStdString();
	auto		slashPos		= pagePathStd.find_last_of('/');

	if(slashPos == std::string::npos)
		return pagePath.toLower();

	slashPos++;

	std::string	lastSegment		= pagePathStd.substr(slashPos),
				firstSegment	= pagePathStd.substr(0, slashPos);

	return QString::fromStdString(firstSegment + stringUtils::toLower(lastSegment));
}

void HelpModel::showOrTogglePage(QString pagePath)
{
	pagePath = convertPagePathToLower(pagePath);

	if(pagePath == _pagePath && _visible)
		setVisible(false);
	else
	{
		setPagePath(pagePath);
		setVisible(true);
	}
}

void HelpModel::reloadPage()
{
	QString curPage = _pagePath;

	if(curPage != "" && curPage != "index")
	{
		setPagePath("index");
		setPagePath(curPage);
	}
}
