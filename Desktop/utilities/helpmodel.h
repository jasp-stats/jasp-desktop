#ifndef HELPMODEL_H
#define HELPMODEL_H

#include <QObject>
#include "languagemodel.h"
#include "analysis/analysis.h"

/// This class is a bit of a mess. Would be good to move all helpfiles to `info` so that this can be cleaned up.
/// It used to support loading a file from Resources/analyses/ANALYSIS_NAME.md/html and show it in HelpModel.qml
/// Then for https://github.com/jasp-stats/INTERNAL-jasp/issues/740 `info` fields were added to qml (and rudimentary to R)
/// These can be converted into markdown on the fly and are more easily translated through weblate but we currently need to support both.
/// This because the info feature isnt entirely finished and also because almost no help file has been ported to these fields now. 
class HelpModel : public QObject
{
	Q_OBJECT
	Q_PROPERTY(bool		visible		READ visible	WRITE setVisible	NOTIFY visibleChanged	)
	Q_PROPERTY(QString	pagePath	READ pagePath	WRITE setPagePath	NOTIFY pagePathChanged	)
	Q_PROPERTY(QString	markdown	READ markdown	WRITE setMarkdown	NOTIFY markdownChanged	)

public:
			HelpModel(QObject * parent);

	bool	visible()	const { return _visible;  }
	QString pagePath()	const { return _pagePath; }
	QString markdown()	const { return _markdown; }
	void	runJavaScript(QString renderFunc, QString content);

public slots:
	void	setVisible(bool visible);
	void	setPagePath(QString pagePath);
	void	setAnalysispagePath(QString analysisName) { setPagePath("analyses/" + analysisName); }
	void	generateJavascript();
	void	showOrTogglePage(QString pagePath);
	void	showOrTogglePageForAnalysis(Analysis * analysis)	{ showOrToggleParticularPageForAnalysis(analysis, ""); }
	void	showOrToggleParticularPageForAnalysis(Analysis * analysis, QString helpPage);
	QString	indexURL();
	void	reloadPage();
	void	setThemeCss(QString themeName);
	void	setFont();
	void	loadingSucceeded();
	void	setMarkdown(QString markdown);
	void	loadMarkdown(QString md);
	bool	pageExists(QString pagePath);

signals:
	void renderCode(QString javascript);
	void visibleChanged(bool visible);
	void pagePathChanged(QString pagePath);
	void runJavaScriptSignal(QString helpJS);

	void markdownChanged(QString markdown);

private:
	QString convertPagePathToLower(const QString & pagePath);
	bool loadHelpContent(const QString & pagePath, bool ignorelanguage, QString &renderFunc, QString &content);

private:
	bool		_visible	= false;
	QString		_pagePath	= "",
				_markdown	= "";
	Analysis *	_analysis	= nullptr;
};

#endif // HELPMODEL_H
