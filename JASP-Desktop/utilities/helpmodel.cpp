#include "helpmodel.h"
#include "appdirs.h"
#include <QFile>
#include <QFileInfo>
#include <QDir>
#include "stringutils.h"
#include "gui/preferencesmodel.h"
#include "log.h"

HelpModel::HelpModel(QObject * parent) : QObject(parent)
{
	setPagePath("index");
	connect(this,						&HelpModel::pagePathChanged,				this, &HelpModel::generateJavascript);
	connect(PreferencesModel::prefs(),	&PreferencesModel::currentThemeNameChanged, this, &HelpModel::setThemeCss,			Qt::QueuedConnection);
	connect(PreferencesModel::prefs(),	&PreferencesModel::realResultFontChanged,	this, &HelpModel::setFont,				Qt::QueuedConnection);
	connect(this,						&HelpModel::markdownChanged,				this, &HelpModel::loadMarkdown);
}

void HelpModel::runJavaScript(QString renderFunc, QString content)
{
#ifdef JASP_DEBUG
	Log::log() << "Help is sending content: '" << content << "'" << std::endl;
#endif

	content.replace("\"", "\\\"");
	content.replace("\r\n", "\\n");
	content.replace("\r", "\\n");
	content.replace("\n", "\\n");



	runJavaScriptSignal(renderFunc + "(\"" + content + "\");");
}

void HelpModel::setVisible(bool visible)
{
	if (_visible == visible)
		return;

	_visible = visible;
	emit visibleChanged(_visible);

}

void HelpModel::loadingSucceeded()
{
	setThemeCss(PreferencesModel::prefs()->currentThemeName());
	setFont();
	generateJavascript();
}

void HelpModel::setMarkdown(QString markdown)
{
	if (_markdown == markdown)
		return;

	_markdown = markdown;
	emit markdownChanged(_markdown);
}

void HelpModel::setPagePath(QString pagePath)
{
	pagePath = convertPagePathToLower(pagePath); //Otherwise we get into to trouble on systems that discern between cases in the filesystem. Also means we should make sure all documentation html are named lowercase!

	_pagePath = pagePath;
	emit pagePathChanged(_pagePath);
}

QString	HelpModel::indexURL()
{
	return "file:" + AppDirs::help() + "/index.html";
}

void HelpModel::generateJavascript()
{
	if(markdown() != "")
	{
		loadMarkdown(markdown());
		return;
	}

	QString renderFunc = "";
	QString content = "";

	//Try to load laguage specific translated help file first.
	if (!loadHelpContent(_pagePath, false, renderFunc, content))
	{
		//Fall back to English
		if (!loadHelpContent(_pagePath, true, renderFunc, content))
		{
			content = tr("Coming Soon!\n========\n\nThere is currently no help available for this analysis.\n\nAdditional documentation will be available in future releases of JASP.");
#ifdef JASP_DEBUG
			content += " (" + _pagePath + ")";
#endif
		}
	}

	runJavaScript(renderFunc, content);
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


void HelpModel::setThemeCss(QString themeName)
{
	runJavaScript("window.setTheme", themeName);
}

void HelpModel::setFont()
{
	QString fontFamily = PreferencesModel::prefs()->realResultFont();
	runJavaScript("window.setFont", fontFamily);
}

bool HelpModel::loadHelpContent(const QString &pagePath, bool ignorelanguage, QString &renderFunc, QString &content)
{

	QString _localname = ignorelanguage ? "" : LanguageModel::currentTranslationSuffix();
	bool found = false;

	renderFunc = "window.render";
	content = "";

	QFile fileMD, fileHTML;
	QFileInfo pathMd(_pagePath + _localname + ".md");

	bool relative = pathMd.isRelative();

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
		found = true;

	}
	else if (fileMD.exists())
	{
		fileMD.open(QFile::ReadOnly);
		content = QString::fromUtf8(fileMD.readAll());
		fileMD.close();
		found = true;
	}

	return found;
}

void HelpModel::loadMarkdown(QString md)
{
	//Log::log() << "loadMarkdown got:\n" << md << std::endl;

	setVisible(true);
	runJavaScript("window.render", md);
}
